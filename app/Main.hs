{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main) where

import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import TypedQL.Row
import TypedQL.Schema

-- | Esquema de exemplo, declarado no nivel de tipos.
type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  ]

-- Projecao valida: o compilador aceita.
-- A anotacao consome o tipo, entao a familia realmente reduz.
projecaoOk :: (All Show (Project ["vendor_code", "open_rate"] Vendors)) => Proxy Vendors
projecaoOk = Proxy

-- | Uma linha concreta desse esquema. A ordem e os tipos sao ditados por
-- 'Vendors': trocar qualquer um deles nao compila.
linha :: Row Vendors
linha = RCons "VFAKE" (RCons "Fornecedor Falso" (RCons 0.42 (RCons 7 RNil)))

main :: IO ()
main = do
  putStrLn "TypedQL 0.1 - modulos 1 (Schema) e 2 (Row)"
  putStrLn ""
  putStrLn "Reflexao (tipo -> valor):"
  print (demote (singSqlType @TDouble))
  putStrLn ""
  putStrLn "Reificacao (valor -> tipo, preso em existencial):"
  print (parseSqlType "text")
  print (parseSqlType "blob")
  putStrLn ""
  putStrLn "Eliminador do existencial:"
  case parseSqlType "int" of
    Nothing -> putStrLn "  tipo desconhecido"
    Just some -> putStrLn ("  dentro da continuacao o tipo e " ++ withSqlType some show)
  putStrLn ""
  putStrLn ("Projecao validada em compile time: " ++ show (const True projecaoOk))
  putStrLn ""
  putStrLn "--- modulo 2: Row ---"
  putStrLn ""
  putStrLn "Cabecalho refletido do esquema:"
  mapM_ (\(n, t) -> putStrLn ("  " ++ n ++ " : " ++ show t)) (header (schemaSing @Vendors))
  putStrLn ""
  putStrLn "Acesso por nome, resolvido em compile time:"
  putStrLn ("  col @\"vendor_name\" = " ++ T.unpack (col @"vendor_name" linha))
  putStrLn ("  col @\"open_rate\"   = " ++ show (col @"open_rate" linha))
  putStrLn ("  col @\"defeitos\"    = " ++ show (col @"defeitos" linha))
  putStrLn ""
  putStrLn "Linha inteira, percorrida com All Show:"
  print (showRow (schemaSing @Vendors) linha)
