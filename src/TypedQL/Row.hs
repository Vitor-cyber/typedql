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
    -- * Singleton do esquema inteiro
  , SSchema (..)
  , KnownSchema (..)
  , header
  , showRow
  , SomeRow (..)
  , withRow
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import TypedQL.Schema

-- | Uma linha do esquema @s@.
--
-- @RCons@ carrega um valor de tipo @Interp t@, ou seja, o tipo Haskell que o
-- esquema manda. Trocar a ordem das colunas ou o tipo de uma delas nao compila.
type Row :: Schema -> Type
data Row s where
  RNil :: Row '[]
  RCons :: Interp t -> Row s -> Row ((n := t) : s)

infixr 5 `RCons`

-- | Prova de que a coluna @n@, de tipo @t@, esta no esquema @s@.
--
-- Note que o tipo @t@ aparece no indice do GADT em vez de ser calculado por
-- 'TypeOf'. Isso e proposital: se escrevessemos o resultado como
-- @Interp (TypeOf n s)@, o GHC travaria no caso recursivo, porque com um
-- esquema abstrato ele nao sabe decidir se a cabeca da lista e ou nao a coluna
-- procurada, e as duas equacoes de 'Lookup' ficam empatadas. Carregar @t@ na
-- prova elimina o problema.
type Index :: Symbol -> SqlType -> Schema -> Type
data Index n t s where
  Here :: Index n t ((n := t) : s)
  There :: Index n t s -> Index n t (c : s)

-- | Constroi a prova por inducao nas instancias.
--
-- A dependencia funcional @n s -> t@ diz que nome e esquema determinam o tipo,
-- e e o que permite escrever @col \@"open_rate" linha@ sem anotar o tipo do
-- resultado. A instancia geral e OVERLAPPABLE, entao o GHC prefere a primeira
-- quando a cabeca casa.
class KnownIndex n t s | n s -> t where
  index :: Index n t s

instance KnownIndex n t ((n := t) : s) where
  index = Here

instance {-# OVERLAPPABLE #-} KnownIndex n t s => KnownIndex n t (c : s) where
  index = There index

-- | Consome a prova andando na linha. Note que nao existe caso de falha:
-- a prova garante que a coluna existe, entao o padrao e exaustivo.
getAt :: Index n t s -> Row s -> Interp t
getAt Here (RCons x _) = x
getAt (There i) (RCons _ r) = getAt i r

-- | Acesso por nome. Uso: @col \@"open_rate" linha@.
--
-- 'AllowAmbiguousTypes' e necessario porque @n@ so aparece na restricao, e
-- 'TypeApplications' e o que permite escolher a coluna. Custo em runtime: zero,
-- a prova e apagada na compilacao.
col :: forall n t s. KnownIndex n t s => Row s -> Interp t
col = getAt (index @n @t @s)

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
    SSchema s ->
    SSchema ((n := t) : s)

-- | Dependencia tipo -> valor, para o esquema completo.
class KnownSchema s where
  schemaSing :: SSchema s

instance KnownSchema '[] where
  schemaSing = SEmpty

instance (KnownSymbol n, KnownSqlType t, KnownSchema s) => KnownSchema ((n := t) : s) where
  schemaSing = SColumn (Proxy @n) (singSqlType @t) (schemaSing @s)

-- | Reflexao do esquema: devolve o cabecalho como valores comuns.
header :: SSchema s -> [(String, SqlType)]
header SEmpty = []
header (SColumn p t rest) = (symbolVal p, demote t) : header rest

-- | Renderiza uma linha percorrendo o singleton do esquema.
--
-- Aqui a restricao 'All' do modulo 1 aparece trabalhando: ela produz uma
-- @Show (Interp t)@ para cada coluna, sem que a gente precise listar as colunas.
showRow :: All Show s => SSchema s -> Row s -> [String]
showRow SEmpty RNil = []
showRow (SColumn _ _ rest) (RCons x xs) = show x : showRow rest xs

-- | Existencial de linha, para quando o esquema so e conhecido em runtime
-- (modulo 6). O eliminador segue a convencao @withX@ das listas.
type SomeRow :: Type
data SomeRow where
  SomeRow :: KnownSchema s => SSchema s -> Row s -> SomeRow

withRow :: SomeRow -> (forall s. KnownSchema s => SSchema s -> Row s -> b) -> b
withRow (SomeRow s r) f = f s r
