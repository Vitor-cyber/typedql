{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 1: o esquema de uma tabela vive no nivel de tipos.
--
-- A ideia central: um esquema e uma lista de pares (nome da coluna, tipo SQL),
-- e essa lista existe apenas em tempo de compilacao. Nenhum valor da linguagem
-- carrega o esquema em runtime, mas o compilador consegue responder perguntas
-- sobre ele: existe a coluna X? qual o tipo dela? essa projecao e valida?
module TypedQL.Schema
  ( -- * Tipos SQL e esquemas
    SqlType (..)
  , Column (..)
  , Schema
    -- * Funcoes no nivel de tipos
  , Interp
  , Lookup
  , TypeOf
  , Names
  , Project
  , Rename
    -- * Restricoes
  , HasColumn
  , Disjoint
  , All
    -- * Singletons (escritos a mao)
  , SSqlType (..)
  , KnownSqlType (..)
  , SomeSqlType (..)
  , withSqlType
  , demote
  , parseSqlType
  ) where

import Data.Kind (Constraint, Type)
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- | Os tipos escalares que o TypedQL entende.
--
-- Declarado com @data@ e nao com @type data@ porque precisamos das duas metades:
-- o kind, para indexar esquemas, e os valores, para construir singletons e
-- reificar o catalogo lido do disco (modulo 6).
data SqlType
  = TInt
  | TDouble
  | TText
  | TBool
  deriving (Show, Eq)

-- | Uma coluna e um par nome/tipo. Nunca construimos um valor desse tipo,
-- ele so nos interessa promovido: @"open_rate" := TDouble@ tem kind 'Column'.
data Column = Symbol := SqlType

infixr 6 :=

-- | Um esquema e uma lista de colunas, no nivel de tipos.
type Schema = [Column]

-- | Interpretacao: leva um 'SqlType' (que aqui aparece como kind) ao tipo
-- Haskell correspondente. E o unico elo entre o esquema e os dados.
-- Familia injetiva, entao o GHC tambem infere o tipo SQL a partir do tipo Haskell.
type Interp :: SqlType -> Type
type family Interp t = r | r -> t where
  Interp TInt = Int
  Interp TDouble = Double
  Interp TText = T.Text
  Interp TBool = Bool

-- | Busca parcial: devolve Nothing se a coluna nao existe.
--
-- Familia fechada, entao as equacoes sao testadas de cima para baixo e a
-- sobreposicao entre a segunda e a terceira e permitida. O padrao nao linear
-- (o mesmo @n@ repetido) e legal em type family, ao contrario do que vale para
-- funcoes de valores.
type Lookup :: Symbol -> Schema -> Maybe SqlType
type family Lookup n s where
  Lookup _ '[] = Nothing
  Lookup n ((n := t) : _) = Just t
  Lookup n (_ : rest) = Lookup n rest

-- | Busca total: se a coluna nao existe o programa nao compila, e a mensagem
-- lista as colunas disponiveis em vez de mostrar uma familia nao reduzida.
type TypeOf :: Symbol -> Schema -> SqlType
type family TypeOf n s where
  TypeOf n s = Unwrap n s (Lookup n s)

type Unwrap :: Symbol -> Schema -> Maybe SqlType -> SqlType
type family Unwrap n s m where
  Unwrap _ _ (Just t) = t
  Unwrap n s Nothing =
    TypeError
      ( Text "TypedQL: a coluna " :<>: ShowType n :<>: Text " nao existe neste esquema."
          :$$: Text "Colunas disponiveis: " :<>: ShowType (Names s)
      )

-- | Os nomes das colunas de um esquema.
type Names :: Schema -> [Symbol]
type family Names s where
  Names '[] = '[]
  Names ((n := _) : rest) = n : Names rest

-- | Projecao no nivel de tipos: o esquema resultante de um SELECT.
type Project :: [Symbol] -> Schema -> Schema
type family Project ns s where
  Project '[] _ = '[]
  Project (n : ns) s = (n := TypeOf n s) : Project ns s

-- | Renomeia uma coluna preservando o tipo.
type Rename :: Symbol -> Symbol -> Schema -> Schema
type family Rename old new s where
  Rename _ _ '[] = '[]
  Rename old new ((old := t) : rest) = (new := t) : rest
  Rename old new (c : rest) = c : Rename old new rest

-- | "O esquema contem a coluna n com tipo t", como restricao.
type HasColumn :: Symbol -> SqlType -> Schema -> Constraint
type HasColumn n t s = Lookup n s ~ Just t

-- | Dois esquemas sao disjuntos quando nao compartilham nomes de coluna.
-- Pre-requisito de uma juncao sem ambiguidade (modulo 4).
type Disjoint :: Schema -> Schema -> Constraint
type family Disjoint a b where
  Disjoint '[] _ = ()
  Disjoint ((n := _) : rest) b = (NotIn n b, Disjoint rest b)

type NotIn :: Symbol -> Schema -> Constraint
type family NotIn n s where
  NotIn n s = NotInAux n (Lookup n s)

type NotInAux :: Symbol -> Maybe SqlType -> Constraint
type family NotInAux n m where
  NotInAux _ Nothing = ()
  NotInAux n (Just _) =
    TypeError
      (Text "TypedQL: juncao ambigua, a coluna " :<>: ShowType n :<>: Text " aparece nos dois lados.")

-- | Propaga uma classe por todas as colunas do esquema.
--
-- Uso de 'ConstraintKinds': a familia devolve algo de kind 'Constraint'.
-- E o que permite derivar Show e Eq para uma linha inteira sem conhecer o
-- esquema, e tambem o que forca a reducao de 'TypeOf' nos testes negativos.
type All :: (Type -> Constraint) -> Schema -> Constraint
type family All c s where
  All _ '[] = ()
  All c ((_ := t) : rest) = (c (Interp t), All c rest)

-- | Singleton de 'SqlType': para cada tipo do kind existe exatamente um valor.
-- E a ponte usada no modulo 6 para transformar um valor lido em runtime em um
-- tipo conhecido pelo compilador.
type SSqlType :: SqlType -> Type
data SSqlType t where
  STInt :: SSqlType TInt
  STDouble :: SSqlType TDouble
  STText :: SSqlType TText
  STBool :: SSqlType TBool

instance Show (SSqlType t) where
  show = show . demote

-- | Dependencia tipo -> valor: dado o tipo, recuperamos o singleton.
class KnownSqlType t where
  singSqlType :: SSqlType t

instance KnownSqlType TInt where singSqlType = STInt
instance KnownSqlType TDouble where singSqlType = STDouble
instance KnownSqlType TText where singSqlType = STText
instance KnownSqlType TBool where singSqlType = STBool

-- | Existencial: esconde o indice quando ele so sera conhecido em runtime.
type SomeSqlType :: Type
data SomeSqlType where
  SomeSqlType :: SSqlType t -> SomeSqlType

instance Show SomeSqlType where
  show (SomeSqlType s) = show s

-- | Eliminador do existencial, no estilo das listas da disciplina.
-- O tipo @t@ nasce e morre dentro da continuacao, ele nao escapa.
withSqlType :: SomeSqlType -> (forall t. SSqlType t -> b) -> b
withSqlType (SomeSqlType s) f = f s

-- | Reflexao: do tipo para o valor.
demote :: SSqlType t -> SqlType
demote = \case
  STInt -> TInt
  STDouble -> TDouble
  STText -> TText
  STBool -> TBool

-- | Reificacao: do valor para o tipo. O tipo resultante nao aparece na
-- assinatura, ele fica preso dentro do existencial.
parseSqlType :: T.Text -> Maybe SomeSqlType
parseSqlType t = case T.toLower (T.strip t) of
  "int" -> Just (SomeSqlType STInt)
  "double" -> Just (SomeSqlType STDouble)
  "text" -> Just (SomeSqlType STText)
  "bool" -> Just (SomeSqlType STBool)
  _ -> Nothing
