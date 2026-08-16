{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: o GHC nao pode provar HasColumn "vendor_code" s para um 's'
-- universal. Dentro de withSomeTable, 's' e uma variavel introduzida pelo
-- eliminador e desconhecida para o chamador. O 'col @"vendor_code"' exige
-- HasColumn "vendor_code" s, que so valeria se 's' fosse, digamos, Vendors --
-- e isso o compilador nao pode assumir.
--
-- Este negativo demonstra o preco do existencial: ganhamos a capacidade de
-- guardar tabelas de esquemas diferentes numa mesma lista, mas perdemos o
-- acesso tipado por nome de coluna sem abrir o existencial.
module AcessoColunaDoExistencial where

import qualified Data.Text as T
import TypedQL.Frontend.Dynamic
import TypedQL.Row (col)
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "open_rate"   := TDouble
  ]

-- Tentativa de extrair vendor_code de um SomeTable.
-- Nao compila: 's' e universal dentro da continuacao; o GHC nao sabe que
-- 's' = Vendors, entao nao pode satisfazer HasColumn "vendor_code" s.
ruim :: SomeTable -> T.Text
ruim t = withSomeTable t (\_ rows -> col @"vendor_code" (head rows))
