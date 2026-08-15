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
-- A ideia central: um esquema e uma lista de colunas, e essa lista existe apenas
-- em tempo de compilacao. Nenhum valor da linguagem carrega o esquema em runtime,
-- mas o compilador consegue responder perguntas sobre ele: existe a coluna X?
-- qual o tipo dela? ela aceita NULL? essa projecao e valida?
module TypedQL.Schema
  ( -- * Tipos SQL, nulabilidade e esquemas
    SqlType (..)
  , Nullability (..)
  , Column (..)
  , type (:=)
  , type (:?)
  , Schema
    -- * Funcoes no nivel de tipos
  , Interp
  , Result
  , Slot
  , ColName
  , ColSqlType
  , ColNull
  , Lookup
  , ColumnOf
  , TypeOf
  , NullabilityOf
  , Names
  , Project
  , Rename
  , MakeNullable
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
  , SNullability (..)
  , KnownNullability (..)
  , demoteNull
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

-- | Uma coluna aceita NULL ou nao. Em SQL isso e uma propriedade do esquema, e
-- aqui ela sobe para o nivel de tipos junto com o resto.
data Nullability
  = NotNull
  | Nullable
  deriving (Show, Eq)

-- | Uma coluna e um nome, um tipo e uma nulabilidade. Nunca construimos um valor
-- desse tipo, ele so nos interessa promovido.
data Column = Col Symbol SqlType Nullability

-- | Coluna obrigatoria: @"open_rate" := TDouble@.
type (:=) :: Symbol -> SqlType -> Column
type n := t = Col n t NotNull

-- | Coluna que aceita NULL: @"cnpj" :? TText@.
type (:?) :: Symbol -> SqlType -> Column
type n :? t = Col n t Nullable

infixr 6 :=
infixr 6 :?

-- | Um esquema e uma lista de colunas, no nivel de tipos.
type Schema = [Column]

-- | Interpretacao: leva um 'SqlType' (que aqui aparece como kind) ao tipo
-- Haskell correspondente. Familia injetiva, entao o GHC tambem infere o tipo SQL
-- a partir do tipo Haskell.
type Interp :: SqlType -> Type
type family Interp t = r | r -> t where
  Interp TInt = Int
  Interp TDouble = Double
  Interp TText = T.Text
  Interp TBool = Bool

-- | O tipo Haskell de um valor de tipo SQL @t@ com nulabilidade @nl@.
--
-- E aqui que a nulabilidade deixa de ser anotacao e passa a ter consequencia:
-- o que nao aceita NULL guarda o valor cru, o que aceita guarda 'Maybe'. Quem le
-- algo nulavel e obrigado pelo compilador a tratar o 'Nothing'. Repare que nao
-- existe custo: uma coluna obrigatoria nao paga o construtor 'Just'.
type Result :: Nullability -> SqlType -> Type
type family Result nl t where
  Result NotNull t = Interp t
  Result Nullable t = Maybe (Interp t)

-- | O tipo Haskell que ocupa a posicao de uma coluna numa linha.
type Slot :: Column -> Type
type family Slot c where
  Slot (Col _ t nl) = Result nl t

-- | Projecoes de uma coluna. Sao usadas no lugar de casamento de padrao direto
-- porque @:=@ e @:?@ sao sinonimos: casar com @n := t@ dentro da biblioteca
-- descartaria silenciosamente as colunas nulaveis.
type ColName :: Column -> Symbol
type family ColName c where
  ColName (Col n _ _) = n

type ColSqlType :: Column -> SqlType
type family ColSqlType c where
  ColSqlType (Col _ t _) = t

type ColNull :: Column -> Nullability
type family ColNull c where
  ColNull (Col _ _ nl) = nl

-- | Busca parcial: devolve Nothing se a coluna nao existe.
--
-- Familia fechada, entao as equacoes sao testadas de cima para baixo e a
-- sobreposicao entre a segunda e a terceira e permitida. O padrao nao linear
-- (o mesmo @n@ repetido) e legal em type family, ao contrario do que vale para
-- funcoes de valores.
type Lookup :: Symbol -> Schema -> Maybe Column
type family Lookup n s where
  Lookup _ '[] = Nothing
  Lookup n (Col n t nl : _) = Just (Col n t nl)
  Lookup n (_ : rest) = Lookup n rest

-- | Busca total: se a coluna nao existe o programa nao compila, e a mensagem
-- lista as colunas disponiveis em vez de mostrar uma familia nao reduzida.
type ColumnOf :: Symbol -> Schema -> Column
type family ColumnOf n s where
  ColumnOf n s = Unwrap n s (Lookup n s)

type Unwrap :: Symbol -> Schema -> Maybe Column -> Column
type family Unwrap n s m where
  Unwrap _ _ (Just c) = c
  Unwrap n s Nothing =
    TypeError
      ( Text "TypedQL: a coluna " :<>: ShowType n :<>: Text " nao existe neste esquema."
          :$$: Text "Colunas disponiveis: " :<>: ShowType (Names s)
      )

-- | O tipo SQL de uma coluna do esquema.
type TypeOf :: Symbol -> Schema -> SqlType
type family TypeOf n s where
  TypeOf n s = ColSqlType (ColumnOf n s)

-- | A nulabilidade de uma coluna do esquema.
type NullabilityOf :: Symbol -> Schema -> Nullability
type family NullabilityOf n s where
  NullabilityOf n s = ColNull (ColumnOf n s)

-- | Os nomes das colunas de um esquema.
type Names :: Schema -> [Symbol]
type family Names s where
  Names '[] = '[]
  Names (c : rest) = ColName c : Names rest

-- | Projecao no nivel de tipos: o esquema resultante de um SELECT.
-- Preserva tipo e nulabilidade, porque copia a coluna inteira.
type Project :: [Symbol] -> Schema -> Schema
type family Project ns s where
  Project '[] _ = '[]
  Project (n : ns) s = ColumnOf n s : Project ns s

-- | Renomeia uma coluna preservando tipo e nulabilidade.
type Rename :: Symbol -> Symbol -> Schema -> Schema
type family Rename old new s where
  Rename _ _ '[] = '[]
  Rename old new (Col old t nl : rest) = Col new t nl : rest
  Rename old new (c : rest) = c : Rename old new rest

-- | Torna todas as colunas nulaveis. E o efeito de um LEFT JOIN no lado direito:
-- se nao houve casamento, todas aquelas colunas viram NULL (modulo 4).
type MakeNullable :: Schema -> Schema
type family MakeNullable s where
  MakeNullable '[] = '[]
  MakeNullable (Col n t _ : rest) = Col n t Nullable : MakeNullable rest

-- | "O esquema contem a coluna n com tipo t", como restricao.
type HasColumn :: Symbol -> SqlType -> Schema -> Constraint
type HasColumn n t s = TypeOf n s ~ t

-- | Dois esquemas sao disjuntos quando nao compartilham nomes de coluna.
-- Pre-requisito de uma juncao sem ambiguidade (modulo 4).
type Disjoint :: Schema -> Schema -> Constraint
type family Disjoint a b where
  Disjoint '[] _ = ()
  Disjoint (c : rest) b = (NotIn (ColName c) b, Disjoint rest b)

type NotIn :: Symbol -> Schema -> Constraint
type family NotIn n s where
  NotIn n s = NotInAux n (Lookup n s)

type NotInAux :: Symbol -> Maybe Column -> Constraint
type family NotInAux n m where
  NotInAux _ Nothing = ()
  NotInAux n (Just _) =
    TypeError
      (Text "TypedQL: juncao ambigua, a coluna " :<>: ShowType n :<>: Text " aparece nos dois lados.")

-- | Propaga uma classe por todas as colunas do esquema.
--
-- Uso de 'ConstraintKinds': a familia devolve algo de kind 'Constraint'.
-- E o que permite derivar Show e Eq para uma linha inteira sem conhecer o
-- esquema, e tambem o que forca a reducao de 'ColumnOf' nos testes negativos.
type All :: (Type -> Constraint) -> Schema -> Constraint
type family All c s where
  All _ '[] = ()
  All c (col : rest) = (c (Slot col), All c rest)

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

-- | Singleton de 'Nullability', mesma construcao de 'SSqlType'.
type SNullability :: Nullability -> Type
data SNullability nl where
  SNotNull :: SNullability NotNull
  SNullable :: SNullability Nullable

instance Show (SNullability nl) where
  show = show . demoteNull

class KnownNullability nl where
  singNullability :: SNullability nl

instance KnownNullability NotNull where singNullability = SNotNull
instance KnownNullability Nullable where singNullability = SNullable

demoteNull :: SNullability nl -> Nullability
demoteNull = \case
  SNotNull -> NotNull
  SNullable -> Nullable
