{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 4: algebra relacional tipada com estagios fantasma.
--
-- A pergunta deste modulo e dupla. Primeira: e possivel construir um plano de
-- consulta onde as operacoes de composicao -- selecao, projecao, juncao -- sejam
-- verificadas em compile time? Segunda: e possivel distinguir um plano logico
-- (ainda nao otimizado) de um plano fisico (pronto para executar), de forma que
-- chamar o executor numa consulta nao compilada simplesmente nao seja um programa
-- valido?
--
-- A resposta a primeira pergunta e o GADT 'QueryNode'. A segunda e o estagio
-- fantasma: o tipo 'Query st s' tem um indice @st@ que e puramente de tipo, nao
-- carrega nenhum valor em runtime. Mas e o suficiente para tornar 'eval'
-- inaplicavel a um plano logico.
--
-- Nota de design: 'compile' e trivial por enquanto (so muda o indice de tipo).
-- O modulo 7 vai substituir isso por uma passagem de otimizacao real
-- (catamorfismo sobre 'Fix QueryF').
module TypedQL.Algebra
  ( -- * Estagios
    Stage (..)
    -- * Plano de consulta
  , Query
  , QueryNode (..)
    -- * Construtores de plano (devolvem Logical)
  , fromTable
  , select
  , project
  , innerJoin
  , leftJoin
    -- * Compilacao e execucao
  , compile
  , CanExecute
  , eval
    -- * Impressao
  , renderSQL
    -- * Classes auxiliares de runtime
  , ProjectRow (..)
  , NullRow (..)
  , ToNullable (..)
  , KnownSymbols (..)
  ) where

import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import TypedQL.Expr (Predicate, evalWhere, renderExpr, toMaybe)
import TypedQL.Row
import TypedQL.Schema

-- | O estagio de um plano: logico (arvore tal como o usuario escreveu) ou fisico
-- (anotada pelo otimizador, pronta para executar).
data Stage = Logical | Physical deriving (Show, Eq)

-- | O no interno da arvore de consulta. Os tipos @l@ e @r@ em 'Join' e 'LJoin'
-- sao variaveis existenciais: elas nao aparecem no indice de resultado ('Append'
-- e 'MakeNullable' as consomem). O GHC desempacota os dicionarios de restricao
-- quando casa o padrao; o avaliador ve os dicionarios, nao os tipos crus.
--
-- O predicado de 'LJoin' e escrito sobre o esquema nao-nulavel do lado direito
-- (@Append l r@, nao @Append l (MakeNullable r)@), porque ele e avaliado contra
-- as linhas reais antes de decidir se ha correspondencia.
type QueryNode :: Schema -> Type
data QueryNode s where
  Table  :: String -> [Row s] -> QueryNode s
  Filter :: Predicate s -> QueryNode s -> QueryNode s
  Pick   ::
    (ProjectRow ns s, KnownSymbols ns) =>
    Proxy ns -> QueryNode s -> QueryNode (Project ns s)
  Join   ::
    Disjoint l r =>
    Predicate (Append l r) -> QueryNode l -> QueryNode r ->
    QueryNode (Append l r)
  LJoin  ::
    (Disjoint l r, NullRow r, ToNullable r) =>
    Predicate (Append l r) -> QueryNode l -> QueryNode r ->
    QueryNode (Append l (MakeNullable r))

-- | Uma consulta sobre o esquema @s@, no estagio @st@.
-- E um 'newtype' sobre 'QueryNode': em runtime nao ha diferenca entre 'Logical'
-- e 'Physical'. A diferenca e puramente de tipo.
type Query :: Stage -> Schema -> Type
newtype Query (st :: Stage) (s :: Schema) = UnsafeQuery (QueryNode s)

-- | Construtores de plano. Todos devolvem 'Logical': o usuario so pode construir
-- planos logicos. 'compile' e o unico caminho para 'Physical'.

fromTable :: String -> [Row s] -> Query Logical s
fromTable name rows = UnsafeQuery (Table name rows)

select :: Predicate s -> Query st s -> Query st s
select p (UnsafeQuery q) = UnsafeQuery (Filter p q)

project ::
  (ProjectRow ns s, KnownSymbols ns) =>
  Proxy ns ->
  Query st s ->
  Query st (Project ns s)
project ns (UnsafeQuery q) = UnsafeQuery (Pick ns q)

innerJoin ::
  Disjoint l r =>
  Predicate (Append l r) ->
  Query st l ->
  Query st r ->
  Query st (Append l r)
innerJoin p (UnsafeQuery l) (UnsafeQuery r) = UnsafeQuery (Join p l r)

leftJoin ::
  (Disjoint l r, NullRow r, ToNullable r) =>
  Predicate (Append l r) ->
  Query st l ->
  Query st r ->
  Query st (Append l (MakeNullable r))
leftJoin p (UnsafeQuery l) (UnsafeQuery r) = UnsafeQuery (LJoin p l r)

-- | Muda o estagio de 'Logical' para 'Physical'. E uma identidade em runtime:
-- o plano fisico tem exatamente a mesma arvore que o logico. O modulo 7 vai
-- substituir isso por transformacoes reais (reordenacao de juncoes, push-down de
-- predicados, selecao de indice).
compile :: Query Logical s -> Query Physical s
compile (UnsafeQuery q) = UnsafeQuery q

-- | "Este estagio permite execucao".
-- A instancia de 'Logical' existe mas o contexto e um 'TypeError': ela nunca pode
-- ser usada. Ela existe apenas para o GHC ter uma mensagem propria para dar, em
-- vez de um erro de unificacao generico. O mesmo truque de 'Total' no modulo 3:
-- uma instancia que existe so para falhar bem.
type CanExecute :: Stage -> Constraint
class CanExecute st

instance CanExecute Physical

instance
  TypeError
    ( Text "TypedQL: esta consulta ainda nao foi compilada."
        :$$: Text "Aplique 'compile' antes de executar."
    ) =>
  CanExecute Logical

-- | Avalia um plano fisico produzindo uma lista de linhas.
eval :: CanExecute st => Query st s -> [Row s]
eval (UnsafeQuery q) = evalNode q

evalNode :: QueryNode s -> [Row s]
evalNode = \case
  Table _ rows ->
    rows
  Filter p q ->
    filter (`evalWhere` p) (evalNode q)
  Pick (_ :: Proxy ns) q ->
    map (projectRow @ns) (evalNode q)
  Join p ql qr ->
    [ appendRow rl rr
    | rl <- evalNode ql
    , rr <- evalNode qr
    , evalWhere (appendRow rl rr) p
    ]
  LJoin p (ql :: QueryNode l) (qr :: QueryNode r) ->
    concatMap
      ( \rl ->
          let rightRows = evalNode qr
              matched =
                [ appendRow rl (toNullable rr)
                | rr <- rightRows
                , evalWhere (appendRow rl rr) p
                ]
          in if null matched
               then [appendRow rl (nullRow @r)]
               else matched
      )
      (evalNode ql)

-- | Imprime a arvore de consulta como pseudo-SQL.
renderSQL :: Query st s -> String
renderSQL (UnsafeQuery q) = renderNode q

renderNode :: QueryNode s -> String
renderNode = \case
  Table name _ ->
    name
  Filter p q ->
    "SELECT * FROM (" ++ renderNode q ++ ") WHERE " ++ renderExpr p
  Pick (_ :: Proxy ns) q ->
    "SELECT "
      ++ intercalate ", " (symbolVals @ns)
      ++ " FROM ("
      ++ renderNode q
      ++ ")"
  Join p ql qr ->
    "SELECT * FROM ("
      ++ renderNode ql
      ++ ") INNER JOIN ("
      ++ renderNode qr
      ++ ") ON "
      ++ renderExpr p
  LJoin p ql qr ->
    "SELECT * FROM ("
      ++ renderNode ql
      ++ ") LEFT JOIN ("
      ++ renderNode qr
      ++ ") ON "
      ++ renderExpr p

-- ---------------------------------------------------------------------------
-- Classes auxiliares de runtime
-- ---------------------------------------------------------------------------

-- | Reflexao de uma lista de simbolos de tipo para uma lista de strings.
-- Usada pelo renderizador de 'Pick' para imprimir os nomes das colunas.
-- Precisa de 'AllowAmbiguousTypes' porque @ns@ nao aparece no tipo de retorno;
-- 'TypeApplications' e a forma de chamada.
type KnownSymbols :: [Symbol] -> Constraint
class KnownSymbols ns where
  symbolVals :: [String]

instance KnownSymbols '[] where
  symbolVals = []

instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n : ns) where
  symbolVals = symbolVal (Proxy @n) : symbolVals @ns

-- | Projecao de uma linha: extrai as colunas de @ns@ do esquema @s@, em ordem.
--
-- A instancia recursiva usa 'col' para cada coluna, o que carrega o custo de
-- uma prova de pertinencia ('KnownIndex') por coluna projetada. Esse custo e de
-- compilacao, nao de execucao: 'col' e apagado pelo compilador apos a checagem.
type ProjectRow :: [Symbol] -> Schema -> Constraint
class ProjectRow ns s where
  projectRow :: Row s -> Row (Project ns s)

instance ProjectRow '[] s where
  projectRow _ = RNil

instance (KnownIndex n (ColumnOf n s) s, ProjectRow ns s) =>
    ProjectRow (n : ns) s where
  projectRow r = RCons (col @n r) (projectRow @ns r)

-- | Constroi uma linha de NULLs para o esquema @s@.
-- Usada no lado direito de um LEFT JOIN quando nenhuma linha direita casa.
type NullRow :: Schema -> Constraint
class NullRow s where
  nullRow :: Row (MakeNullable s)

instance NullRow '[] where
  nullRow = RNil

instance NullRow rest => NullRow (Col n t nl : rest) where
  nullRow = RCons Nothing (nullRow @rest)

-- | Embrulha cada valor de uma linha em 'Just', subindo para a versao nulavel.
-- Usada nas linhas do lado direito que casaram num LEFT JOIN.
type ToNullable :: Schema -> Constraint
class ToNullable s where
  toNullable :: Row s -> Row (MakeNullable s)

instance ToNullable '[] where
  toNullable _ = RNil

instance (KnownNullability nl, ToNullable rest) =>
    ToNullable (Col n t nl : rest) where
  toNullable (RCons x rest) =
    RCons (toMaybe (singNullability @nl) x) (toNullable rest)
