{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 3: expressoes escalares tipadas, com nulabilidade no tipo.
--
-- Um SGBD comete tres erros classicos ao avaliar uma expressao: comparar coisas
-- de tipos incompativeis, somar texto, e esquecer que um valor pode ser NULL.
-- Os dois primeiros o esquema no nivel de tipos ja resolve. O terceiro e o
-- assunto deste modulo.
--
-- A ideia: o tipo de uma expressao tem tres indices, o esquema em que ela faz
-- sentido, o tipo SQL do resultado e a nulabilidade do resultado. A nulabilidade
-- e calculada, nao declarada: se qualquer operando pode ser NULL, o resultado
-- pode ser NULL. E um WHERE so aceita expressao que nao pode ser NULL, porque um
-- filtro tem que decidir entre verdadeiro e falso.
module TypedQL.Expr
  ( -- * A expressao
    Expr (..)
  , MergeNull
    -- * Construtores convenientes
  , colE
    -- * Avaliacao
  , evalExpr
  , Predicate
  , Total (..)
  , evalWhere
    -- * Utilitarios de nulabilidade
  , toMaybe
  , mapResult
  , liftBin
  , liftBool
  , kleeneAnd
  , kleeneOr
    -- * Impressao
  , renderExpr
  ) where

import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)
import TypedQL.Row
import TypedQL.Schema

-- | Combina a nulabilidade de dois operandos. E o coracao do modulo: em SQL,
-- qualquer operando NULL contamina o resultado.
--
-- Familia fechada com uma equacao especifica e um curinga. O curinga so e
-- alcancado quando a primeira equacao e comprovadamente inaplicavel, entao
-- @MergeNull Nullable nl@ reduz para 'Nullable' mesmo com @nl@ desconhecido.
type MergeNull :: Nullability -> Nullability -> Nullability
type family MergeNull a b where
  MergeNull NotNull NotNull = NotNull
  MergeNull _ _ = Nullable

-- | Expressao escalar sobre o esquema @s@, de tipo SQL @t@ e nulabilidade @nl@.
--
-- Cada construtor carrega as restricoes que a avaliacao vai precisar. Isso e
-- possivel porque um GADT guarda dicionarios: @ELit@ exige 'Show' na construcao e
-- entrega 'Show' na desconstrucao, e as operacoes binarias exigem
-- 'KnownNullability' porque o avaliador precisa saber, em runtime, se cada lado
-- e um 'Maybe' ou nao.
type Expr :: Schema -> SqlType -> Nullability -> Type
data Expr s t nl where
  -- | Literal. Nunca e NULL.
  ELit :: Show (Interp t) => Interp t -> Expr s t NotNull
  -- | O NULL de um tipo. O singleton diz de que tipo, para a impressao.
  ENull :: SSqlType t -> Expr s t Nullable
  -- | Referencia a uma coluna. Tipo e nulabilidade vem do esquema, via a prova
  -- de pertinencia do modulo 2: nao existe coluna inventada.
  ECol :: (KnownSymbol n, KnownIndex n (Col n t nl) s) => Proxy n -> Expr s t nl
  ENot :: KnownNullability nl => Expr s TBool nl -> Expr s TBool nl
  EAnd ::
    (KnownNullability n1, KnownNullability n2) =>
    Expr s TBool n1 ->
    Expr s TBool n2 ->
    Expr s TBool (MergeNull n1 n2)
  EOr ::
    (KnownNullability n1, KnownNullability n2) =>
    Expr s TBool n1 ->
    Expr s TBool n2 ->
    Expr s TBool (MergeNull n1 n2)
  -- | Igualdade homogenea: os dois lados tem o mesmo tipo SQL. Comparar texto com
  -- numero nao e um erro de runtime, e um programa que nao existe.
  EEq ::
    (Eq (Interp t), KnownNullability n1, KnownNullability n2) =>
    Expr s t n1 ->
    Expr s t n2 ->
    Expr s TBool (MergeNull n1 n2)
  ELt ::
    (Ord (Interp t), KnownNullability n1, KnownNullability n2) =>
    Expr s t n1 ->
    Expr s t n2 ->
    Expr s TBool (MergeNull n1 n2)
  EAdd ::
    (Num (Interp t), KnownNullability n1, KnownNullability n2) =>
    Expr s t n1 ->
    Expr s t n2 ->
    Expr s t (MergeNull n1 n2)
  -- | A valvula de escape: @IS NULL@ nunca e NULL. E o unico jeito de sair da
  -- nulabilidade sem inventar um valor.
  EIsNull :: KnownNullability nl => Expr s t nl -> Expr s TBool NotNull
  -- | A outra valvula: fornece um padrao para o caso NULL.
  ECoalesce :: KnownNullability nl => Expr s t nl -> Expr s t NotNull -> Expr s t NotNull

-- | @colE \@"open_rate"@ em vez de @ECol (Proxy \@"open_rate")@.
colE :: forall n s t nl. (KnownSymbol n, KnownIndex n (Col n t nl) s) => Expr s t nl
colE = ECol (Proxy @n)

-- | Avalia a expressao numa linha.
--
-- O tipo do resultado e **calculado**: @Result NotNull t@ e o valor cru,
-- @Result Nullable t@ e um 'Maybe'. Uma expressao que nao pode ser NULL nao paga
-- nada em runtime, nem alocacao de 'Just' nem teste de 'Nothing'.
-- Duas coisas explicam a verbosidade daqui para baixo.
--
-- Primeira: 'Result' nao e injetiva. De @Result nl t@ o compilador nao consegue
-- voltar para @nl@ e @t@, entao ele nao adivinha a nulabilidade a partir do valor
-- avaliado: cada chamada precisa dizer, com aplicacao de tipos, qual e.
--
-- Segunda: os construtores binarios escondem as nulabilidades dos operandos
-- (elas nao aparecem no tipo do resultado, apenas dentro de 'MergeNull'), logo
-- sao variaveis existenciais. O jeito de dar nome a elas no GHC 9.6 e uma
-- assinatura de padrao no sub-padrao, como em @(a :: Expr s TBool n1)@.
evalExpr :: forall s t nl. Row s -> Expr s t nl -> Result nl t
evalExpr r = \case
  ELit x -> x
  ENull _ -> Nothing
  ECol (_ :: Proxy n) -> getAt (index @n @(Col n t nl) @s) r
  ENot e ->
    mapResult @nl @TBool @TBool singNullability not (evalExpr r e)
  EAnd (a :: Expr s TBool n1) (b :: Expr s TBool n2) ->
    liftBool @n1 @n2 singNullability singNullability (&&) kleeneAnd (evalExpr r a) (evalExpr r b)
  EOr (a :: Expr s TBool n1) (b :: Expr s TBool n2) ->
    liftBool @n1 @n2 singNullability singNullability (||) kleeneOr (evalExpr r a) (evalExpr r b)
  EEq (a :: Expr s u n1) (b :: Expr s u n2) ->
    liftBin @n1 @n2 @u @u @TBool singNullability singNullability (==) (evalExpr r a) (evalExpr r b)
  ELt (a :: Expr s u n1) (b :: Expr s u n2) ->
    liftBin @n1 @n2 @u @u @TBool singNullability singNullability (<) (evalExpr r a) (evalExpr r b)
  EAdd (a :: Expr s t n1) (b :: Expr s t n2) ->
    liftBin @n1 @n2 @t @t @t singNullability singNullability (+) (evalExpr r a) (evalExpr r b)
  EIsNull (e :: Expr s u nlE) ->
    isNothing (toMaybe @nlE @u singNullability (evalExpr r e))
  ECoalesce (e :: Expr s t nlE) d ->
    fromMaybe (evalExpr r d) (toMaybe @nlE @t singNullability (evalExpr r e))

-- | Um filtro e uma expressao booleana que nao pode ser NULL.
type Predicate :: Schema -> Type
type Predicate s = Expr s TBool NotNull

-- | "Esta nulabilidade e total", ou seja, decide.
--
-- A instancia de 'Nullable' existe, mas o contexto dela e um 'TypeError': ela
-- nunca pode ser usada, serve apenas para o GHC ter uma mensagem propria para
-- dar em vez de @Couldn't match Nullable with NotNull@. O corpo e inalcancavel.
type Total :: Nullability -> Constraint
class Total nl where
  totalResult :: Result nl t -> Interp t

instance Total NotNull where
  totalResult = id

instance
  TypeError
    ( Text "TypedQL: este filtro pode ser NULL, entao ele nao decide nada."
        :$$: Text "Um WHERE precisa escolher entre verdadeiro e falso."
        :$$: Text "Trate o NULL antes, com EIsNull ou ECoalesce."
    ) =>
  Total Nullable
  where
  totalResult = error "TypedQL: inalcancavel, a instancia existe apenas pela mensagem"

-- | Avalia um filtro. Se a expressao puder ser NULL o programa nao compila.
evalWhere :: forall s nl. Total nl => Row s -> Expr s TBool nl -> Bool
evalWhere r e = totalResult @nl @TBool (evalExpr r e)

-- | Apaga a diferenca entre os dois casos de 'Result', para poder combinar.
toMaybe :: forall nl t. SNullability nl -> Result nl t -> Maybe (Interp t)
toMaybe SNotNull x = Just x
toMaybe SNullable x = x

-- | Aplica uma funcao preservando a nulabilidade.
mapResult :: forall nl a b. SNullability nl -> (Interp a -> Interp b) -> Result nl a -> Result nl b
mapResult SNotNull f x = f x
mapResult SNullable f x = fmap f x

-- | Combina dois resultados. O caso @NotNull NotNull@ nao passa por 'Maybe'
-- nenhum: e a mesma aplicacao de funcao que haveria em codigo comum.
liftBin ::
  forall n1 n2 a b c.
  SNullability n1 ->
  SNullability n2 ->
  (Interp a -> Interp b -> Interp c) ->
  Result n1 a ->
  Result n2 b ->
  Result (MergeNull n1 n2) c
liftBin SNotNull SNotNull f x y = f x y
liftBin SNotNull SNullable f x y = fmap (f x) y
liftBin SNullable s2 f x y = do
  x' <- x
  y' <- toMaybe s2 y
  pure (f x' y')

-- | Igual a 'liftBin', mas para os conectivos booleanos, que em SQL nao sao
-- estritos: @FALSE AND NULL@ e @FALSE@, nao @NULL@.
--
-- Por isso a funcao recebe **duas** implementacoes: a total, para quando nenhum
-- lado e nulavel, e a de tres valores, para os outros casos. O tipo obriga a
-- fornecer a versao total; nao ha como se livrar dela com um @fromJust@.
liftBool ::
  forall n1 n2.
  SNullability n1 ->
  SNullability n2 ->
  (Bool -> Bool -> Bool) ->
  (Maybe Bool -> Maybe Bool -> Maybe Bool) ->
  Result n1 TBool ->
  Result n2 TBool ->
  Result (MergeNull n1 n2) TBool
liftBool SNotNull SNotNull estrita _ x y = estrita x y
liftBool SNotNull SNullable _ tresValores x y = tresValores (Just x) y
liftBool SNullable s2 _ tresValores x y = tresValores x (toMaybe s2 y)

-- | Logica de tres valores do SQL.
kleeneAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
kleeneAnd (Just False) _ = Just False
kleeneAnd _ (Just False) = Just False
kleeneAnd (Just True) (Just True) = Just True
kleeneAnd _ _ = Nothing

kleeneOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
kleeneOr (Just True) _ = Just True
kleeneOr _ (Just True) = Just True
kleeneOr (Just False) (Just False) = Just False
kleeneOr _ _ = Nothing

-- | Imprime a expressao como SQL. So e possivel porque os construtores guardam
-- os dicionarios ('Show' no literal, 'KnownSymbol' na coluna).
renderExpr :: Expr s t nl -> String
renderExpr = \case
  ELit x -> show x
  ENull t -> "NULL::" ++ show t
  ECol p -> symbolVal p
  ENot e -> "NOT " ++ renderExpr e
  EAnd a b -> "(" ++ renderExpr a ++ " AND " ++ renderExpr b ++ ")"
  EOr a b -> "(" ++ renderExpr a ++ " OR " ++ renderExpr b ++ ")"
  EEq a b -> "(" ++ renderExpr a ++ " = " ++ renderExpr b ++ ")"
  ELt a b -> "(" ++ renderExpr a ++ " < " ++ renderExpr b ++ ")"
  EAdd a b -> "(" ++ renderExpr a ++ " + " ++ renderExpr b ++ ")"
  EIsNull e -> "(" ++ renderExpr e ++ " IS NULL)"
  ECoalesce e d -> "COALESCE(" ++ renderExpr e ++ ", " ++ renderExpr d ++ ")"
