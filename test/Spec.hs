{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main) where

import Data.Proxy (Proxy (..))
import Test.Tasty
import Test.Tasty.HUnit
import TypedQL.Row
import TypedQL.Schema

type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  ]

linha :: Row Vendors
linha = RCons "VFAKE" (RCons "Fornecedor Teste Um" (RCons 0.42 (RCons 17 RNil)))

-- Testes no nivel de tipos: se estas definicoes compilam, a propriedade vale.
-- Cada uma e uma igualdade de tipos verificada pelo GHC, nada roda em runtime.
tipoDaColuna :: Proxy (TypeOf "open_rate" Vendors) -> Proxy TDouble
tipoDaColuna = id

nomesDoEsquema :: Proxy (Names Vendors) -> Proxy ["vendor_code", "vendor_name", "open_rate", "defeitos"]
nomesDoEsquema = id

projecaoPreservaTipo ::
  Proxy (Project '["open_rate"] Vendors) -> Proxy '["open_rate" := TDouble]
projecaoPreservaTipo = id

renomeiaPreservaTipo ::
  Proxy (Rename "open_rate" "taxa" Vendors) ->
  Proxy ["vendor_code" := TText, "vendor_name" := TText, "taxa" := TDouble, "defeitos" := TInt]
renomeiaPreservaTipo = id

interpEInjetiva :: Proxy (Interp TInt) -> Proxy Int
interpEInjetiva = id

main :: IO ()
main = defaultMain $ testGroup "TypedQL"
  [ testGroup "Schema, nivel de tipos"
      [ testCase "TypeOf devolve o tipo da coluna" $ const () (tipoDaColuna Proxy) @?= ()
      , testCase "Names lista os nomes na ordem" $ const () (nomesDoEsquema Proxy) @?= ()
      , testCase "Project preserva o tipo da coluna" $ const () (projecaoPreservaTipo Proxy) @?= ()
      , testCase "Rename troca o nome e mantem o tipo" $ const () (renomeiaPreservaTipo Proxy) @?= ()
      , testCase "Interp e injetiva" $ const () (interpEInjetiva Proxy) @?= ()
      ]
  , testGroup "Schema, nivel de valores"
      [ testCase "demote e a inversa do singleton" $
          demote STInt @?= TInt
      , testCase "parseSqlType ignora caixa" $
          fmap show (parseSqlType "TEXT") @?= Just "TText"
      , testCase "parseSqlType rejeita nome invalido" $
          maybe True (const False) (parseSqlType "blob") @?= True
      , testCase "withSqlType elimina o existencial" $
          fmap (\s -> withSqlType s show) (parseSqlType "bool") @?= Just "TBool"
      ]
  , testGroup "Row"
      [ testCase "col devolve Text na coluna de texto" $
          col @"vendor_code" linha @?= "VFAKE"
      , testCase "col devolve Double na coluna numerica" $
          col @"open_rate" linha @?= 0.42
      , testCase "col devolve Int na ultima coluna" $
          col @"defeitos" linha @?= 17
      , testCase "header reflete o esquema inteiro" $
          header (schemaSing @Vendors)
            @?= [ ("vendor_code", TText)
                , ("vendor_name", TText)
                , ("open_rate", TDouble)
                , ("defeitos", TInt)
                ]
      , testCase "showRow percorre a linha usando All Show" $
          showRow (schemaSing @Vendors) linha
            @?= ["\"VFAKE\"", "\"Fornecedor Teste Um\"", "0.42", "17"]
      , testCase "withRow elimina o existencial de linha" $
          withRow (SomeRow (schemaSing @Vendors) linha) (\s _ -> length (header s)) @?= 4
      ]
  ]
