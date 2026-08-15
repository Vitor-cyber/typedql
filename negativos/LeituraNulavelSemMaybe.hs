{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: ler uma coluna nulavel devolve @Maybe Text@, nunca @Text@. Este e
-- o lado importante da garantia: nao existe caminho que entregue o valor de uma
-- coluna nulavel como se a ausencia nao pudesse acontecer.
module LeituraNulavelSemMaybe where

import Data.Text (Text)
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "cnpj" :? TText
  ]

linha :: Row Vendors
linha = RCons "VFAKE" (RCons (Just "00.000.000/0001-00") RNil)

-- col @"cnpj" tem tipo Maybe Text; a assinatura pede Text
uso :: Text
uso = col @"cnpj" linha
