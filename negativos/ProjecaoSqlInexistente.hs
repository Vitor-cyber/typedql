{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: o quasiquoter gera project (Proxy @'["taxa"]) ... e o GHC
-- rejeita porque "taxa" nao e coluna do esquema. A garantia do modulo 2 (nao
-- existe coluna inventada) atravessa o frontend de SQL: escrever SQL com uma
-- coluna errada nao produz um erro de runtime, produz um programa que nao existe.
module ProjecaoSqlInexistente where

import TypedQL.Algebra
import TypedQL.Frontend.Static (sql)
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "open_rate" := TDouble
  ]

vendorsQ :: Query Logical Vendors
vendorsQ = fromTable "vendors" []

-- "taxa" nao existe: o nome certo e "open_rate".
ruim :: Query Logical (Project '["taxa"] Vendors)
ruim = [sql| SELECT taxa FROM vendorsQ |]
