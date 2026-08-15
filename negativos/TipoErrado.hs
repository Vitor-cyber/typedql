{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: open_rate e TDouble, o codigo exige TInt.
module TipoErrado where

import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  ]

-- so funciona se open_rate for inteira
contagem :: HasColumn "open_rate" TInt Vendors => Int
contagem = 0

uso :: Int
uso = contagem
