{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: fromTable devolve Query Logical, mas eval exige Physical.
-- A mensagem explica que 'compile' precisa ser chamado antes.
module EvalSemCompilar where

import TypedQL.Algebra
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors = '["vendor_code" := TText]

uso :: [Row Vendors]
uso = eval (fromTable "vendors" [])
