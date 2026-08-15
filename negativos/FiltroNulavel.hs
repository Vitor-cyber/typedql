{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: o filtro compara uma coluna nulavel, entao ele pode ser NULL, e
-- um WHERE que devolve NULL nao decide nada. O tipo recusa antes de rodar.
module FiltroNulavel where

import TypedQL.Expr
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "cnpj" :? TText
  ]

linha :: Row Vendors
linha = RCons "VFAKE" (RCons Nothing RNil)

-- Em SQL isto compila e devolve zero linha sem avisar ninguem.
-- Aqui nao compila: falta tratar o NULL com EIsNull ou ECoalesce.
uso :: Bool
uso = evalWhere linha (EEq (colE @"cnpj") (ELit "00.000.000/0001-00"))
