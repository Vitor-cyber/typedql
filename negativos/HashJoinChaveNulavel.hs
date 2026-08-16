{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- ESTE ARQUIVO NAO DEVE COMPILAR.
-- Nota: os operandos sao anotados porque 'Append' nao e injetiva; sem isso o GHC
-- reclamaria de ambiguidade em vez de reclamar da chave, e o negativo passaria
-- pelo motivo errado.
-- Erro esperado: hash join com chave que aceita NULL.
-- Em SQL, NULL = NULL nao e verdadeiro, entao uma linha de chave NULL nunca casa.
-- Um hash join ingenuo coloca o NULL na tabela de hash, encontra correspondencia
-- e devolve linhas que a semantica do SQL diz que nao existem. Aqui o construtor
-- exige ColumnOf k s ~ Col k t NotNull, entao esse operador nao pode ser criado.
module HashJoinChaveNulavel where

import TypedQL.Engine
import TypedQL.Schema

type A :: Schema
type A = ["a_id" := TInt, "a_cnpj" :? TText]

type B :: Schema
type B = '["b_cnpj" := TText]

-- a_cnpj aceita NULL: nao serve como chave de hash join.
juncaoInvalida :: PhysOp (Append A B)
juncaoInvalida =
  hashJoin @"a_cnpj" @"b_cnpj" (scan "a" [] :: PhysOp A) (scan "b" [] :: PhysOp B)
