{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: as duas tabelas tem a coluna vendor_code, a juncao seria ambigua.
module JuncaoAmbigua where

import TypedQL.Schema

type Vendors =
  '[ "vendor_code" ':= 'TText
   , "vendor_name" ':= 'TText
   , "open_rate" ':= 'TDouble
   , "defeitos" ':= 'TInt
   ]

type Metricas =
  '[ "vendor_code" ':= 'TText
   , "pedidos" ':= 'TInt
   ]

juntavel :: Disjoint Vendors Metricas => Bool
juntavel = True

uso :: Bool
uso = juntavel
