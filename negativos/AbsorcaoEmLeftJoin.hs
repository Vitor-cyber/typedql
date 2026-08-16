{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: a absorcao do WHERE na condicao do LEFT JOIN e valida em INNER
-- JOIN e ERRADA em LEFT JOIN. Aqui isso nao e uma regra que o programador tem que
-- lembrar: o predicado do LEFT JOIN vive no esquema nao-nulavel do lado direito
-- (Append A B) e o filtro de cima vive no nulavel (Append A (MakeNullable B)).
-- Sao tipos diferentes, entao a reescrita errada nao existe como programa.
module AbsorcaoEmLeftJoin where

import TypedQL.Algebra
import TypedQL.Expr
import TypedQL.Schema

type A :: Schema
type A = '["a_code" := TText]

type B :: Schema
type B = '["b_code" := TText]

-- Condicao do ON: fala das colunas reais do lado direito.
condicaoOn :: Predicate (Append A B)
condicaoOn = EEq (colE @"a_code") (colE @"b_code")

-- Filtro de cima: fala do lado direito DEPOIS do LEFT JOIN, onde tudo e nulavel.
filtroDeCima :: Predicate (Append A (MakeNullable B))
filtroDeCima = EIsNull (colE @"b_code")

-- A reescrita invalida: juntar os dois num AND.
absorveNoOn :: QueryNode (Append A (MakeNullable B))
absorveNoOn = LJoin (EAnd condicaoOn filtroDeCima) (Table "a" []) (Table "b" [])
