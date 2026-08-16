{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Erro esperado: uma reescrita de plano tem que preservar o esquema do resultado.
-- Esta aqui descarta a projecao, entao o plano de saida tem o esquema de ENTRADA
-- (s) onde o tipo exige o esquema PROJETADO (Project ns s). O contrato do
-- otimizador esta no tipo de QueryF r s -> r s, nao num teste.
module OtimizadorMudaEsquema where

import TypedQL.Algebra (QueryNode)
import TypedQL.Optimize

reescritaRuim :: QueryF QueryNode s -> QueryNode s
reescritaRuim (PickF _ filho) = filho
reescritaRuim outro = into outro
