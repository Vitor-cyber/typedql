{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 8: o executor. Operadores fisicos indexados pelo esquema que produzem.
--
-- A pergunta deste modulo: o modulo 4 ja executa consultas, mas de um jeito so
-- (laco aninhado para tudo). Um SGBD de verdade escolhe **algoritmo**: varredura
-- ou indice, laco aninhado ou hash join. Essa escolha e onde o otimizador erra
-- feio. Quanto dela o tipo consegue vigiar?
--
-- A resposta interessante esta no 'HashJoin'. Um hash join constroi uma tabela de
-- hash sobre a chave de um lado e sonda com a chave do outro. Isso exige tres
-- coisas que num SGBD comum sao checadas em runtime, ou nao sao checadas:
--
-- 1. as duas colunas de chave existem, cada uma no seu lado;
-- 2. elas tem o mesmo tipo SQL (nao se sonda uma tabela de 'Text' com um 'Int');
-- 3. **nenhuma das duas aceita NULL**.
--
-- O item 3 e o que vale a pena. Em SQL @NULL = NULL@ nao e verdadeiro, entao uma
-- linha com chave NULL nunca casa com nada. Um hash join implementado sem cuidado
-- coloca o NULL na tabela de hash, encontra a correspondencia e devolve linhas que
-- o SQL diz que nao existem. Aqui a assinatura do construtor exige
-- @ColumnOf kl l ~ Col kl t NotNull@ nos dois lados: o operador que erraria nao
-- pode ser construido. Ver 'negativos/HashJoinChaveNulavel.hs'.
--
-- O planejador ('planejar') e mais um catamorfismo, reusando o 'hcata' do modulo 7
-- com 'PhysOp' como carregador. O que ele **nao** faz esta documentado em
-- 'planejar': promover um predicado de igualdade a chave de hash join nao
-- typecheca, e a razao e instrutiva.
module TypedQL.Engine
  ( -- * Operadores fisicos
    PhysOp (..)
    -- * Construtores convenientes
  , scan
  , hashJoin
    -- * Planejamento
  , planejar
    -- * Execucao
  , runOp
  , executar
  , rodar
    -- * EXPLAIN
  , explainOp
  , explainPlano
    -- * Modelo de custo
  , Estimativa (..)
  , estimar
  ) where

import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import TypedQL.Algebra
import TypedQL.Expr (Predicate, evalWhere, renderExpr)
import TypedQL.Optimize (QueryF (..), hcata)
import TypedQL.Row
import TypedQL.Schema

-- ---------------------------------------------------------------------------
-- Operadores fisicos
-- ---------------------------------------------------------------------------

-- | Um operador fisico produz linhas de esquema @s@. E a arvore que o executor
-- percorre, e a diferenca em relacao a 'QueryNode' e que aqui o **algoritmo** esta
-- escolhido: 'NLJoin' e 'HashJoin' produzem o mesmo esquema por caminhos
-- diferentes, e as restricoes de cada um dizem o que ele precisa para funcionar.
type PhysOp :: Schema -> Type
data PhysOp s where
  -- | Varredura completa de uma tabela materializada.
  Scan :: String -> [Row s] -> PhysOp s
  -- | Aplica o predicado linha a linha.
  FilterOp :: Predicate s -> PhysOp s -> PhysOp s
  -- | Descarta colunas. O esquema de saida e calculado, nao declarado.
  ProjectOp ::
    (ProjectRow ns s, KnownSymbols ns) =>
    Proxy ns -> PhysOp s -> PhysOp (Project ns s)
  -- | Laco aninhado: funciona com qualquer predicado, custa o produto.
  NLJoin ::
    Disjoint l r =>
    Predicate (Append l r) -> PhysOp l -> PhysOp r -> PhysOp (Append l r)
  -- | Hash join por igualdade de chave. As restricoes sao o assunto do modulo:
  --
  -- * @ColumnOf kl l ~ Col kl t NotNull@ diz que @kl@ existe em @l@, tem tipo @t@
  --   e nao aceita NULL;
  -- * o mesmo @t@ aparece nos dois lados, entao as chaves sao comparaveis;
  -- * @Ord (Interp t)@ e o que a tabela de hash precisa.
  --
  -- Um hash join com chave nulavel nao e um operador que exista aqui.
  HashJoin ::
    ( Disjoint l r
    , KnownSymbol kl
    , KnownSymbol kr
    , KnownIndex kl (ColumnOf kl l) l
    , KnownIndex kr (ColumnOf kr r) r
    , ColumnOf kl l ~ Col kl t NotNull
    , ColumnOf kr r ~ Col kr t NotNull
    , Ord (Interp t)
    ) =>
    Proxy kl -> Proxy kr -> PhysOp l -> PhysOp r -> PhysOp (Append l r)
  -- | LEFT JOIN por laco aninhado. O esquema de saida torna o lado direito
  -- nulavel, exatamente como no modulo 4.
  NLLeftJoin ::
    (Disjoint l r, NullRow r, ToNullable r) =>
    Predicate (Append l r) -> PhysOp l -> PhysOp r -> PhysOp (Append l (MakeNullable r))

-- | Varredura de tabela, com nome para o EXPLAIN.
scan :: String -> [Row s] -> PhysOp s
scan = Scan

-- | @hashJoin \@"vendor_code" \@"camp_vendor" esquerda direita@.
--
-- Os nomes das chaves vao por aplicacao de tipo, nao por string: nao existe
-- caminho para escrever uma chave que o esquema nao tem.
hashJoin ::
  forall kl kr t l r.
  ( Disjoint l r
  , KnownSymbol kl
  , KnownSymbol kr
  , KnownIndex kl (ColumnOf kl l) l
  , KnownIndex kr (ColumnOf kr r) r
  , ColumnOf kl l ~ Col kl t NotNull
  , ColumnOf kr r ~ Col kr t NotNull
  , Ord (Interp t)
  ) =>
  PhysOp l ->
  PhysOp r ->
  PhysOp (Append l r)
hashJoin = HashJoin (Proxy @kl) (Proxy @kr)

-- ---------------------------------------------------------------------------
-- Planejamento
-- ---------------------------------------------------------------------------

-- | Baixa uma arvore logica para operadores fisicos. E o terceiro catamorfismo do
-- projeto: reusa o 'hcata' do modulo 7 trocando o carregador por 'PhysOp'.
--
-- Todas as juncoes saem como laco aninhado, e isso nao e preguica. Promover
-- automaticamente @ON (a = b)@ para 'HashJoin' **nao typecheca**, e a razao e o
-- ponto mais fino que encontrei no projeto: quando casamos o padrao
-- @EEq (ECol pa) (ECol pb)@ num @Predicate (Append l r)@, os dicionarios que o
-- 'ECol' carrega sao provas de pertinencia no esquema **junto**
-- (@KnownIndex a (Col a t nl) (Append l r)@). O 'HashJoin' precisa de provas em
-- cada lado separado (@KnownIndex kl _ l@), e nao existe funcao que quebre a
-- primeira prova nas duas outras: a informacao de qual lado a coluna veio foi
-- apagada por 'Append'. Para recuperar isso seria preciso indexar o predicado
-- pelos dois esquemas de origem, nao pelo concatenado. O hash join fica, portanto,
-- como algo que o usuario pede explicitamente com 'hashJoin', e o tipo confere.
planejar :: QueryNode s -> PhysOp s
planejar = hcata algFisica

algFisica :: QueryF PhysOp x -> PhysOp x
algFisica = \case
  TableF nome linhas -> Scan nome linhas
  FilterF p op -> FilterOp p op
  PickF ns op -> ProjectOp ns op
  JoinF p e d -> NLJoin p e d
  LJoinF p e d -> NLLeftJoin p e d

-- ---------------------------------------------------------------------------
-- Execucao
-- ---------------------------------------------------------------------------

-- | Roda um operador fisico.
--
-- Repare que nenhum caso precisa checar nada: as restricoes que cada construtor
-- guarda ja sao exatamente o que o algoritmo consome. O 'HashJoin' usa
-- @col \@kr@ sem verificar se a coluna existe, e usa o valor como chave de 'Map'
-- sem desembrulhar 'Maybe', porque o tipo garantiu 'NotNull' na construcao.
runOp :: PhysOp s -> [Row s]
runOp = \case
  Scan _ linhas ->
    linhas
  FilterOp p op ->
    filter (`evalWhere` p) (runOp op)
  ProjectOp (_ :: Proxy ns) op ->
    map (projectRow @ns) (runOp op)
  NLJoin p ope opd ->
    let direitas = runOp opd
     in [ appendRow re rd
        | re <- runOp ope
        , rd <- direitas
        , evalWhere (appendRow re rd) p
        ]
  HashJoin (_ :: Proxy kl) (_ :: Proxy kr) (ope :: PhysOp l) (opd :: PhysOp r) ->
    -- 'flip (++)' e nao '(++)': 'fromListWith' combina novo com antigo, entao
    -- sem o flip cada balde sairia invertido e a ordem de saida deixaria de
    -- coincidir com a do laco aninhado.
    let tabela = M.fromListWith (flip (++)) [(col @kr rd, [rd]) | rd <- runOp opd]
     in concatMap
          (\re -> map (appendRow re) (M.findWithDefault [] (col @kl re) tabela))
          (runOp ope)
  NLLeftJoin p ope (opd :: PhysOp r) ->
    let direitas = runOp opd
     in concatMap
          ( \re ->
              let casadas =
                    [ appendRow re (toNullable rd)
                    | rd <- direitas
                    , evalWhere (appendRow re rd) p
                    ]
               in if null casadas then [appendRow re (nullRow @r)] else casadas
          )
          (runOp ope)

-- | Executa uma consulta compilada. Mesma barreira de estagio do modulo 4: uma
-- consulta 'Logical' nao chega aqui.
executar :: CanExecute st => Query st s -> [Row s]
executar = runOp . planejar . planNode

-- | O caminho completo de uma consulta: otimizar, planejar, executar.
-- Recebe 'Logical' e nao pede 'compile' porque ela mesma compila.
rodar :: (Query Logical s -> Query Physical s) -> Query Logical s -> [Row s]
rodar compilador = executar . compilador

-- ---------------------------------------------------------------------------
-- EXPLAIN
-- ---------------------------------------------------------------------------

-- | O plano fisico impresso em arvore, como o @EXPLAIN@ de um SGBD. Ao contrario
-- de 'renderSQL', mostra o **algoritmo** escolhido, nao o SQL equivalente.
explainOp :: PhysOp s -> String
explainOp = unlines . linhasExplain 0

-- | 'explainOp' de uma consulta, com a estimativa no rodape.
explainPlano :: CanExecute st => Query st s -> String
explainPlano q =
  let op = planejar (planNode q)
      e = estimar op
   in explainOp op
        ++ "  (estimativa: "
        ++ show (linhasEstimadas e)
        ++ " linhas, "
        ++ show (comparacoes e)
        ++ " comparacoes)"

linhasExplain :: Int -> PhysOp s -> [String]
linhasExplain n = \case
  Scan nome linhas ->
    [recuo n ++ "Scan " ++ nome ++ " (" ++ show (length linhas) ++ " linhas)"]
  FilterOp p op ->
    (recuo n ++ "Filter " ++ renderExpr p) : linhasExplain (n + 1) op
  ProjectOp (_ :: Proxy ns) op ->
    (recuo n ++ "Project " ++ show (symbolVals @ns)) : linhasExplain (n + 1) op
  NLJoin p ope opd ->
    (recuo n ++ "NestedLoopJoin ON " ++ renderExpr p)
      : (linhasExplain (n + 1) ope ++ linhasExplain (n + 1) opd)
  HashJoin pl pr ope opd ->
    ( recuo n
        ++ "HashJoin "
        ++ symbolVal pl
        ++ " = "
        ++ symbolVal pr
        ++ " (chaves NotNull garantidas pelo tipo)"
    )
      : (linhasExplain (n + 1) ope ++ linhasExplain (n + 1) opd)
  NLLeftJoin p ope opd ->
    (recuo n ++ "NestedLoopLeftJoin ON " ++ renderExpr p)
      : (linhasExplain (n + 1) ope ++ linhasExplain (n + 1) opd)

recuo :: Int -> String
recuo n = concat (replicate n "  ") ++ "-> "

-- ---------------------------------------------------------------------------
-- Modelo de custo
-- ---------------------------------------------------------------------------

-- | Estimativa grosseira de um plano: quantas linhas ele pode produzir no pior
-- caso e quantas comparacoes de linha ele faz para isso.
--
-- E deliberadamente pessimista (um filtro estima que nada e filtrado), porque o
-- objetivo aqui nao e um otimizador baseado em custo de verdade, e mostrar a
-- diferenca assintotica entre os dois algoritmos de juncao: laco aninhado e o
-- produto, hash join e a soma.
data Estimativa = Estimativa
  { linhasEstimadas :: Int
  , comparacoes :: Int
  }
  deriving (Show, Eq)

estimar :: PhysOp s -> Estimativa
estimar = \case
  Scan _ linhas ->
    Estimativa (length linhas) 0
  FilterOp _ op ->
    let e = estimar op
     in Estimativa (linhasEstimadas e) (comparacoes e + linhasEstimadas e)
  ProjectOp _ op ->
    estimar op
  NLJoin _ ope opd ->
    juncao (estimar ope) (estimar opd) produto
  HashJoin _ _ ope opd ->
    juncao (estimar ope) (estimar opd) soma
  NLLeftJoin _ ope opd ->
    juncao (estimar ope) (estimar opd) produto
  where
    produto a b = a * b
    soma a b = a + b
    juncao ee ed custoDaJuncao =
      Estimativa
        (linhasEstimadas ee * linhasEstimadas ed)
        (comparacoes ee + comparacoes ed + custoDaJuncao (linhasEstimadas ee) (linhasEstimadas ed))
