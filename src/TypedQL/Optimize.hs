{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 7: otimizacao de planos como catamorfismo indexado.
--
-- A pergunta deste modulo: um otimizador de consultas e um programa que reescreve
-- arvores. Ele e a parte de um SGBD onde os bugs mais caros moram, porque um plano
-- reescrito errado nao trava, ele devolve a resposta errada. Quanto dessa classe
-- de bug o sistema de tipos elimina?
--
-- A resposta e o tipo de 'compileWith' no modulo 4:
--
-- > compileWith :: (QueryNode s -> QueryNode s) -> Query Logical s -> Query Physical s
--
-- O @s@ e o mesmo dos dois lados. Uma reescrita que mude o esquema do resultado
-- (que perca uma projecao, que troque a ordem de uma juncao, que esqueca que um
-- LEFT JOIN torna o lado direito nulavel) nao e um programa valido. O otimizador
-- nao precisa de teste para isso, ele precisa de compilador.
--
-- O caso mais bonito e a absorcao do filtro na condicao da juncao. Em SQL,
--
-- > SELECT * FROM a INNER JOIN b ON p WHERE q
--
-- e igual a @... ON (p AND q)@, mas com LEFT JOIN a mesma reescrita esta errada:
-- mover o @q@ para o @ON@ muda quais linhas ganham NULL. Aqui essa distincao nao
-- e uma regra que o programador precisa lembrar. O predicado de um LEFT JOIN tem
-- tipo @Predicate (Append l r)@ e o filtro acima dele tem tipo
-- @Predicate (Append l (MakeNullable r))@: sao tipos diferentes, entao a
-- reescrita errada nao compila. A semantica do SQL virou um erro de tipo.
--
-- Sobre o catamorfismo: 'QueryNode' e recursivo e indexado por 'Schema', logo o
-- @Functor@ base tambem tem que ser indexado. 'QueryF' e uma camada da arvore com
-- um buraco @r :: Schema -> Type@ no lugar dos filhos, e 'hmap' e o @fmap@ desse
-- funtor de indice em indice (natural transformation, nao funcao comum). Nao
-- precisamos de um @Fix@: 'QueryNode' **ja e** o ponto fixo de 'QueryF', e o par
-- 'out' / 'into' e a testemunha do isomorfismo.
module TypedQL.Optimize
  ( -- * O funtor base indexado
    QueryF (..)
  , hmap
  , out
  , into
    -- * Catamorfismo
  , hcata
    -- * O otimizador
  , Otimizado (..)
  , otimizar
  , compileOtimizado
  , explicar
    -- * Metrica
  , tamanhoPlano
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import TypedQL.Algebra
import TypedQL.Expr (Expr (..), Predicate)
import TypedQL.Row (Row)
import TypedQL.Schema

-- ---------------------------------------------------------------------------
-- O funtor base indexado
-- ---------------------------------------------------------------------------

-- | Uma camada da arvore de consulta, com os filhos substituidos por @r@.
--
-- E 'QueryNode' com um buraco. As restricoes de cada construtor sao as mesmas,
-- porque quem consome a camada (o avaliador, o renderizador, o otimizador) precisa
-- dos mesmos dicionarios. Os indices @l@ e @q@ das juncoes continuam existenciais.
type QueryF :: (Schema -> Type) -> Schema -> Type
data QueryF r s where
  TableF :: String -> [Row s] -> QueryF r s
  FilterF :: Predicate s -> r s -> QueryF r s
  PickF ::
    (ProjectRow ns s, KnownSymbols ns) =>
    Proxy ns -> r s -> QueryF r (Project ns s)
  JoinF ::
    Disjoint l q =>
    Predicate (Append l q) -> r l -> r q -> QueryF r (Append l q)
  LJoinF ::
    (Disjoint l q, NullRow q, ToNullable q) =>
    Predicate (Append l q) -> r l -> r q -> QueryF r (Append l (MakeNullable q))

-- | O @fmap@ de 'QueryF'. Repare no tipo do argumento: nao e @f s -> g s@ para um
-- @s@ fixo, e @forall x. f x -> g x@. Tem que ser polimorfica porque os filhos de
-- uma juncao tem indices diferentes do resultado, e sao existenciais: a funcao
-- precisa funcionar para qualquer esquema, inclusive um que nao tem nome.
hmap :: (forall x. f x -> g x) -> QueryF f s -> QueryF g s
hmap k = \case
  TableF nome linhas -> TableF nome linhas
  FilterF p filho -> FilterF p (k filho)
  PickF ns filho -> PickF ns (k filho)
  JoinF p e d -> JoinF p (k e) (k d)
  LJoinF p e d -> LJoinF p (k e) (k d)

-- | Desenrola uma camada. Metade do isomorfismo @QueryNode s ~ QueryF QueryNode s@.
out :: QueryNode s -> QueryF QueryNode s
out = \case
  Table nome linhas -> TableF nome linhas
  Filter p filho -> FilterF p filho
  Pick ns filho -> PickF ns filho
  Join p e d -> JoinF p e d
  LJoin p e d -> LJoinF p e d

-- | Enrola uma camada. A outra metade.
into :: QueryF QueryNode s -> QueryNode s
into = \case
  TableF nome linhas -> Table nome linhas
  FilterF p filho -> Filter p filho
  PickF ns filho -> Pick ns filho
  JoinF p e d -> Join p e d
  LJoinF p e d -> LJoin p e d

-- ---------------------------------------------------------------------------
-- Catamorfismo
-- ---------------------------------------------------------------------------

-- | Catamorfismo indexado: consome a arvore de baixo para cima trocando cada
-- camada pelo resultado da algebra.
--
-- A algebra tambem e polimorfica no indice (@forall x@), pela mesma razao que
-- 'hmap': ela vai ser aplicada aos filhos existenciais de uma juncao. E o tipo
-- diz que ela nao pode mentir sobre o esquema, ela consome uma camada sobre @x@
-- e produz um resultado sobre o mesmo @x@.
hcata :: (forall x. QueryF g x -> g x) -> QueryNode s -> g s
hcata alg = alg . hmap (hcata alg) . out

-- ---------------------------------------------------------------------------
-- O otimizador
-- ---------------------------------------------------------------------------

-- | O carregador do catamorfismo: um plano equivalente ao original mais o registro
-- das reescritas aplicadas. O @s@ do plano e o mesmo @s@ do 'Otimizado', que e o
-- que garante que a otimizacao preserva o esquema.
type Otimizado :: Schema -> Type
data Otimizado s = Otimizado
  { planoOtimizado :: QueryNode s
  , reescritas :: [String]
  }

-- | Roda o otimizador sobre uma arvore.
otimizar :: QueryNode s -> Otimizado s
otimizar = hcata algOtimiza

-- | A algebra. Cada caso e uma regra de reescrita local; o catamorfismo se encarrega
-- de aplica-las em toda a arvore, de baixo para cima, o que faz as regras
-- encadearem sozinhas (tres filtros aninhados fundem em um numa passagem).
algOtimiza :: QueryF Otimizado x -> Otimizado x
algOtimiza = \case
  TableF nome linhas ->
    Otimizado (Table nome linhas) []
  FilterF p (Otimizado filho hist) ->
    reescreveFiltro p filho hist
  PickF ns (Otimizado filho hist) ->
    Otimizado (Pick ns filho) hist
  JoinF p (Otimizado e he) (Otimizado d hd) ->
    Otimizado (Join p e d) (he ++ hd)
  LJoinF p (Otimizado e he) (Otimizado d hd) ->
    Otimizado (LJoin p e d) (he ++ hd)

-- | As regras que envolvem um filtro. Sao quatro:
--
-- 1. filtro constantemente verdadeiro desaparece;
-- 2. filtro constantemente falso curto-circuita a subarvore inteira para vazio;
-- 3. filtros consecutivos fundem em um @AND@;
-- 4. filtro sobre INNER JOIN e absorvido pela condicao de juncao.
--
-- A regra 4 nao tem contraparte para LEFT JOIN, e nao por escolha: veja a nota no
-- cabecalho do modulo. Se voce tentar acrescentar o caso @LJoin@ aqui, o GHC
-- recusa, porque o predicado do LEFT JOIN vive num esquema nao-nulavel e o filtro
-- de cima vive no nulavel.
reescreveFiltro :: Predicate s -> QueryNode s -> [String] -> Otimizado s
reescreveFiltro p filho hist = case constante p of
  Just True ->
    Otimizado filho (hist ++ ["filtro sempre verdadeiro eliminado"])
  Just False ->
    Otimizado (Table "vazio" []) (hist ++ ["filtro sempre falso, subarvore virou vazia"])
  Nothing -> case filho of
    Filter q neto ->
      Otimizado (Filter (EAnd p q) neto) (hist ++ ["filtros consecutivos fundidos em AND"])
    Join q e d ->
      Otimizado (Join (EAnd q p) e d) (hist ++ ["filtro absorvido pela condicao do INNER JOIN"])
    _ ->
      Otimizado (Filter p filho) hist

-- | Avalia uma expressao booleana sem linha nenhuma, quando ela nao depende de
-- coluna. Devolve 'Nothing' quando depende (o caso comum).
--
-- Nao precisa de 'Row' e nao precisa se preocupar com NULL: uma expressao sem
-- 'ECol' e sem 'ENull' e um valor fechado. Os casos que poderiam ser NULL caem no
-- curinga e simplesmente nao sao otimizados, o que e conservador e correto.
constante :: Expr s TBool nl -> Maybe Bool
constante = \case
  ELit b -> Just b
  ENot e -> not <$> constante e
  EAnd a b -> (&&) <$> constante a <*> constante b
  EOr a b -> (||) <$> constante a <*> constante b
  _ -> Nothing

-- | Compila otimizando. Substitui 'compile' na pratica; 'compile' continua
-- exportado para poder comparar o plano logico com o fisico.
compileOtimizado :: Query Logical s -> Query Physical s
compileOtimizado q = compileWith (planoOtimizado . otimizar) q

-- | As reescritas que o otimizador aplicaria neste plano, em ordem.
explicar :: Query st s -> [String]
explicar = reescritas . otimizar . planNode

-- | Numero de nos da arvore. Outro catamorfismo, com carregador constante: e a
-- metrica que mostra que a otimizacao encolheu o plano.
--
-- 'Contagem' existe porque o carregador de 'hcata' tem que ter kind
-- @Schema -> Type@; um 'Int' cru nao serve, precisa de um newtype indexado que
-- ignora o indice.
type Contagem :: Schema -> Type
newtype Contagem s = Contagem Int

tamanhoPlano :: Query st s -> Int
tamanhoPlano q = case hcata algConta (planNode q) of
  Contagem n -> n

algConta :: QueryF Contagem x -> Contagem x
algConta = \case
  TableF _ _ -> Contagem 1
  FilterF _ (Contagem n) -> Contagem (n + 1)
  PickF _ (Contagem n) -> Contagem (n + 1)
  JoinF _ (Contagem a) (Contagem b) -> Contagem (a + b + 1)
  LJoinF _ (Contagem a) (Contagem b) -> Contagem (a + b + 1)
