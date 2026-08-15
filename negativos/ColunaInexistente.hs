{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: a coluna "vendor_cod" nao existe no esquema (erro de digitacao).
--
-- Nota: um Proxy sozinho nao basta, porque o GHC nao reduz uma type family que
-- ninguem consome. Aqui a restricao All Show forca a reducao, exatamente como o
-- modulo Row vai fazer quando precisar do tipo Haskell de cada coluna.
module ColunaInexistente where

import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  ]

consulta :: All Show (Project ["vendor_cod", "open_rate"] Vendors) => Bool
consulta = True

uso :: Bool
uso = consulta
