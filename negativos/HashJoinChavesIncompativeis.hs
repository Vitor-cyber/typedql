{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Nota: os operandos sao anotados porque 'Append' nao e injetiva; sem isso o GHC
-- reclamaria de ambiguidade em vez de reclamar da chave, e o negativo passaria
-- pelo motivo errado.
-- Erro esperado: hash join sondando uma tabela de hash de Text com uma chave Int.
-- O construtor usa o MESMO t nas duas chaves (ColumnOf kl l ~ Col kl t NotNull e
-- ColumnOf kr r ~ Col kr t NotNull), entao chaves de tipos diferentes nao unificam.
module HashJoinChavesIncompativeis where

import TypedQL.Engine
import TypedQL.Schema

type A :: Schema
type A = '["a_code" := TText]

type B :: Schema
type B = '["b_num" := TInt]

juncaoInvalida :: PhysOp (Append A B)
juncaoInvalida =
  hashJoin @"a_code" @"b_num" (scan "a" [] :: PhysOp A) (scan "b" [] :: PhysOp B)
