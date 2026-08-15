{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: nao existe prova de que "taxa" seja coluna do esquema, entao o
-- GHC nao encontra instancia de KnownIndex.
module AcessoAColunaInexistente where

import Data.Text (Text)
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "open_rate" := TDouble
  ]

linha :: Row Vendors
linha = RCons "VFAKE" (RCons 0.42 RNil)

-- "taxa" nao existe: o nome certo e "open_rate"
uso :: Text
uso = col @"taxa" linha
