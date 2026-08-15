{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: as duas tabelas tem a coluna vendor_code, a juncao seria ambigua.
module JuncaoAmbigua where

import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  ]

type Metricas :: Schema
type Metricas =
  [ "vendor_code" := TText
  , "pedidos" := TInt
  ]

juntavel :: Disjoint Vendors Metricas => Bool
juntavel = True

uso :: Bool
uso = juntavel
