{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: a coluna "cnpj" aceita NULL, entao a posicao dela numa linha e um
-- Maybe. Guardar o valor cru nao e permitido.
--
-- O valor vai numa definicao com assinatura, em vez de literal direto, de proposito.
-- Com um literal e 'OverloadedStrings' o GHC reclamaria de falta de instancia
-- @IsString (Maybe Text)@, que e um sintoma indireto. Com um @Text@ nomeado o erro e
-- a incompatibilidade que interessa mostrar.
module ColunaNulavelSemMaybe where

import Data.Text (Text)
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "cnpj" :? TText
  ]

cnpj :: Text
cnpj = "00.000.000/0001-00"

linha :: Row Vendors
linha = RCons "VFAKE" (RCons cnpj RNil)
