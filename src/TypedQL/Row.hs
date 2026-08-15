{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 2: uma linha de tabela indexada pelo esquema.
--
-- 'Row' e uma lista heterogenea: cada posicao tem um tipo diferente, ditado pelo
-- esquema. A mesma ideia do @Stack@ da lista 07, mas em vez de guardar
-- delimitadores guarda valores, e o indice diz o tipo de cada um.
--
-- O ganho: acessar coluna e uma operacao verificada em tempo de compilacao.
-- Nao existe "campo nao encontrado" nem cast em runtime.
module TypedQL.Row
  ( -- * A linha
    Row (..)
    -- * Acesso por nome, verificado em compile time
  , Index (..)
  , KnownIndex (..)
  , getAt
  , col
  , appendRow
    -- * Singleton do esquema inteiro
  , SSchema (..)
  , KnownSchema (..)
  , header
  , showRow
  , SomeRow (..)
  , withRow
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import TypedQL.Schema

-- | Uma linha do esquema @s@.
--
-- @RCons@ carrega um valor de tipo @Slot c@: o valor cru se a coluna e
-- obrigatoria, um 'Maybe' se ela aceita NULL. Trocar a ordem das colunas, o tipo
-- de uma delas ou a nulabilidade nao compila.
type Row :: Schema -> Type
data Row s where
  RNil :: Row '[]
  RCons :: Slot c -> Row s -> Row (c : s)

infixr 5 `RCons`

-- | Prova de que a coluna @c@ esta no esquema @s@.
--
-- Note que a prova carrega a coluna inteira, nao apenas o nome. Isso e
-- proposital: se o tipo de 'getAt' fosse @Index n s -> Row s -> Slot (ColumnOf n s)@,
-- o GHC travaria no caso recursivo, porque com um esquema abstrato ele nao sabe
-- decidir se a cabeca da lista e ou nao a coluna procurada, e as duas equacoes de
-- 'Lookup' ficam empatadas. Carregar a coluna na prova elimina o problema.
--
-- Isso vale para a recursao interna. Na fronteira da API ('col') vale o contrario:
-- ali o esquema e concreto, a familia reduz, e usar 'ColumnOf' produz erro melhor.
-- Ver a nota em 'col'.
type Index :: Column -> Schema -> Type
data Index c s where
  Here :: Index c (c : s)
  There :: Index c s -> Index c (d : s)

-- | Constroi a prova por inducao nas instancias.
--
-- A dependencia funcional @n s -> c@ diz que nome e esquema determinam a coluna,
-- e e o que permite escrever @col \@"open_rate" linha@ sem anotar o tipo do
-- resultado. A instancia geral e OVERLAPPABLE, entao o GHC prefere a primeira
-- quando a cabeca casa.
type KnownIndex :: Symbol -> Column -> Schema -> Constraint
class KnownIndex n c s | n s -> c where
  index :: Index c s

instance KnownIndex n (Col n t nl) (Col n t nl : s) where
  index = Here

-- A aplicacao explicita de tipos e necessaria porque o tipo do metodo, @Index c s@,
-- nao menciona @n@: sem dizer qual coluna estamos procurando, a chamada recursiva
-- fica ambigua. As variaveis do cabecalho da instancia estao no escopo do corpo
-- gracas a 'ScopedTypeVariables'.
instance {-# OVERLAPPABLE #-} KnownIndex n c s => KnownIndex n c (d : s) where
  index = There (index @n @c @s)

-- | Consome a prova andando na linha. Note que nao existe caso de falha:
-- a prova garante que a coluna existe, entao o padrao e exaustivo.
getAt :: Index c s -> Row s -> Slot c
getAt Here (RCons x _) = x
getAt (There i) (RCons _ r) = getAt i r

-- | Concatena duas linhas produzindo uma linha do esquema concatenado.
--
-- Funciona como a concatenacao de listas comuns, mas no nivel dos tipos: ao
-- casar o padrao do GADT, o GHC sabe que o primeiro argumento e @'[]@ (caso
-- 'RNil') ou @c : s1'@ (caso 'RCons'), o que faz 'Append' reduzir na assinatura
-- sem precisar de nenhum helper de prova. Usado pelo modulo 4 nas juncoes.
appendRow :: Row s1 -> Row s2 -> Row (Append s1 s2)
appendRow RNil       r2 = r2
appendRow (RCons x r1) r2 = RCons x (appendRow r1 r2)

-- | Acesso por nome. Uso: @col \@"open_rate" linha@.
--
-- 'AllowAmbiguousTypes' e necessario porque @n@ so aparece na restricao, e
-- 'TypeApplications' e o que permite escolher a coluna. Custo em runtime: zero,
-- a prova e apagada na compilacao.
--
-- Se a coluna for nulavel o resultado e um 'Maybe', e nao ha como esquecer disso:
-- o tipo obriga a tratar.
--
-- A coluna e determinada por 'ColumnOf', nao deixada como variavel resolvida pela
-- dependencia funcional. A diferenca aparece so no caso de erro, e e grande: com
-- uma variavel, um nome inexistente deixa a coluna ambigua e o GHC reclama da
-- ambiguidade ("the type variable c0 is ambiguous"), escondendo a causa. Com
-- 'ColumnOf', a familia nao reduz e dispara a mensagem que diz qual coluna nao
-- existe e quais existem.
col :: forall n s. KnownIndex n (ColumnOf n s) s => Row s -> Slot (ColumnOf n s)
col = getAt (index @n @(ColumnOf n s) @s)

-- | Singleton do esquema inteiro: um valor que espelha a lista de tipos.
--
-- Precisamos dele porque para percorrer uma linha generica alguem tem que dizer
-- em runtime quantas colunas existem e de que tipo. O esquema em si foi apagado
-- pelo compilador, o singleton e a copia que sobrevive.
type SSchema :: Schema -> Type
data SSchema s where
  SEmpty :: SSchema '[]
  SColumn ::
    KnownSymbol n =>
    Proxy n ->
    SSqlType t ->
    SNullability nl ->
    SSchema s ->
    SSchema (Col n t nl : s)

-- | Dependencia tipo -> valor, para o esquema completo.
class KnownSchema s where
  schemaSing :: SSchema s

instance KnownSchema '[] where
  schemaSing = SEmpty

instance
  (KnownSymbol n, KnownSqlType t, KnownNullability nl, KnownSchema s) =>
  KnownSchema (Col n t nl : s)
  where
  schemaSing = SColumn (Proxy @n) (singSqlType @t) (singNullability @nl) (schemaSing @s)

-- | Reflexao do esquema: devolve o cabecalho como valores comuns.
header :: SSchema s -> [(String, SqlType, Nullability)]
header SEmpty = []
header (SColumn p t nl rest) = (symbolVal p, demote t, demoteNull nl) : header rest

-- | Renderiza uma linha percorrendo o singleton do esquema.
--
-- Aqui a restricao 'All' do modulo 1 aparece trabalhando: ela produz uma
-- @Show (Slot c)@ para cada coluna, sem que a gente precise listar as colunas.
showRow :: All Show s => SSchema s -> Row s -> [String]
showRow SEmpty RNil = []
showRow (SColumn _ _ _ rest) (RCons x xs) = show x : showRow rest xs

-- | Existencial de linha, para quando o esquema so e conhecido em runtime
-- (modulo 6). O eliminador segue a convencao @withX@ das listas.
type SomeRow :: Type
data SomeRow where
  SomeRow :: KnownSchema s => SSchema s -> Row s -> SomeRow

withRow :: SomeRow -> (forall s. KnownSchema s => SSchema s -> Row s -> b) -> b
withRow (SomeRow s r) f = f s r
