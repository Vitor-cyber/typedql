{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import TypedQL.Algebra
import TypedQL.Expr
import TypedQL.Row
import TypedQL.Schema

-- | Esquema de exemplo, declarado no nivel de tipos.
-- @:=@ e coluna obrigatoria, @:?@ e coluna que aceita NULL.
type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  , "cnpj" :? TText
  ]

-- Projecao valida: o compilador aceita.
-- A anotacao consome o tipo, entao a familia realmente reduz.
projecaoOk :: (All Show (Project ["vendor_code", "open_rate"] Vendors)) => Proxy Vendors
projecaoOk = Proxy

-- | Duas linhas concretas. A ordem, os tipos e a nulabilidade sao ditados por
-- 'Vendors': trocar qualquer um deles nao compila.
comCnpj :: Row Vendors
comCnpj =
  RCons "VFAKE" (RCons "Fornecedor Falso" (RCons 0.42 (RCons 7 (RCons (Just "00.000.000/0001-00") RNil))))

semCnpj :: Row Vendors
semCnpj =
  RCons "FAKEV" (RCons "Fornecedor Sem Cadastro" (RCons 0.08 (RCons 31 (RCons Nothing RNil))))

-- | Um filtro: taxa de abertura abaixo de 10 por cento e sem CNPJ cadastrado.
--
-- Repare que @EIsNull@ e obrigatorio para o resultado ser um 'Predicate': sem ele
-- a expressao seria nulavel e 'evalWhere' nao compilaria.
filtroCritico :: Predicate Vendors
filtroCritico =
  EAnd
    (ELt (colE @"open_rate") (ELit 0.1))
    (EIsNull (colE @"cnpj"))

-- | Esquema de campanhas de comunicacao. Os nomes sao diferentes de 'Vendors'
-- para que os dois esquemas sejam disjuntos e possam ser juntados.
type Campanhas :: Schema
type Campanhas =
  [ "camp_id" := TInt
  , "camp_vendor" := TText
  ]

-- | Condicao de juncao: vendor_code do lado esquerdo deve igualar camp_vendor
-- do lado direito. O tipo garante que as duas colunas existem e tem o mesmo
-- tipo SQL (TText). Comparar texto com inteiro nao compila.
joinCond :: Predicate (Append Vendors Campanhas)
joinCond = EEq (colE @"vendor_code") (colE @"camp_vendor")

main :: IO ()
main = do
  putStrLn "TypedQL 0.1 - modulos 1 (Schema), 2 (Row), 3 (Expr) e 4 (Algebra)"
  putStrLn ""
  putStrLn "--- modulo 1: Schema ---"
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
  mapM_ mostraColuna (header (schemaSing @Vendors))
  putStrLn ""
  putStrLn "Acesso por nome, resolvido em compile time:"
  putStrLn ("  col @\"vendor_name\" = " ++ T.unpack (col @"vendor_name" comCnpj))
  putStrLn ("  col @\"open_rate\"   = " ++ show (col @"open_rate" comCnpj))
  putStrLn ("  col @\"cnpj\"        = " ++ show (col @"cnpj" comCnpj) ++ "   <- Maybe, porque a coluna e nulavel")
  putStrLn ""
  putStrLn "Linha inteira, percorrida com All Show:"
  print (showRow (schemaSing @Vendors) comCnpj)
  putStrLn ""
  putStrLn "--- modulo 3: Expr ---"
  putStrLn ""
  putStrLn ("Filtro: " ++ renderExpr filtroCritico)
  putStrLn ("  na linha com cnpj: " ++ show (evalWhere comCnpj filtroCritico))
  putStrLn ("  na linha sem cnpj: " ++ show (evalWhere semCnpj filtroCritico))
  putStrLn ""
  putStrLn "O tipo do resultado depende da nulabilidade:"
  putStrLn
    ( "  defeitos + 3        :: Int        = "
        ++ show (evalExpr comCnpj (EAdd (colE @"defeitos") (ELit 3)))
    )
  putStrLn
    ( "  defeitos + NULL     :: Maybe Int  = "
        ++ show (evalExpr comCnpj (EAdd (colE @"defeitos") (ENull STInt)))
    )
  putStrLn ""
  putStrLn "Logica de tres valores do SQL:"
  putStrLn
    ( "  FALSE AND (cnpj = 'x') = "
        ++ show (evalExpr semCnpj (EAnd (ELit False) (EEq (colE @"cnpj") (ELit "x"))))
        ++ "   (nao NULL: FALSE absorve)"
    )
  putStrLn
    ( "  TRUE  AND (cnpj = 'x') = "
        ++ show (evalExpr semCnpj (EAnd (ELit True) (EEq (colE @"cnpj") (ELit "x"))))
    )
  putStrLn ""
  putStrLn "COALESCE tira o Maybe do tipo, nao apenas do valor:"
  putStrLn
    ( "  COALESCE(cnpj, 'sem cadastro') :: Text = "
        ++ T.unpack (evalExpr semCnpj (ECoalesce (colE @"cnpj") (ELit "sem cadastro")))
    )

  let campanhas :: [Row Campanhas]
      campanhas =
        [ RCons 1 (RCons "VFAKE" RNil)
        , RCons 2 (RCons "VFAKE" RNil)
        ]
      tabelaVendors = [comCnpj, semCnpj]

  putStrLn "--- modulo 4: Algebra ---"
  putStrLn ""
  let planoSelecao = select filtroCritico (fromTable "vendors" tabelaVendors)
  putStrLn ("Selecao (SQL): " ++ renderSQL planoSelecao)
  let selecionados = eval (compile planoSelecao)
  putStrLn ("Resultado (" ++ show (length selecionados) ++ " linhas):")
  mapM_ (\r -> putStrLn ("  " ++ T.unpack (col @"vendor_code" r))) selecionados
  putStrLn ""
  let planoJuncao = innerJoin joinCond (fromTable "vendors" tabelaVendors) (fromTable "campanhas" campanhas)
  putStrLn ("INNER JOIN (SQL): " ++ renderSQL planoJuncao)
  let linhasJuncao = eval (compile planoJuncao)
  putStrLn ("Resultado (" ++ show (length linhasJuncao) ++ " linhas; FAKEV sem campanha some):")
  mapM_ (\r -> putStrLn ("  vendor=" ++ T.unpack (col @"vendor_code" r) ++ ", camp_id=" ++ show (col @"camp_id" r))) linhasJuncao
  putStrLn ""
  let planoLeft = leftJoin joinCond (fromTable "vendors" tabelaVendors) (fromTable "campanhas" campanhas)
  putStrLn ("LEFT JOIN (SQL): " ++ renderSQL planoLeft)
  let linhasLeft = eval (compile planoLeft)
  putStrLn ("Resultado (" ++ show (length linhasLeft) ++ " linhas; FAKEV aparece com camp_id=Nothing):")
  mapM_ (\r -> putStrLn ("  vendor=" ++ T.unpack (col @"vendor_code" r) ++ ", camp_id=" ++ show (col @"camp_id" r))) linhasLeft

mostraColuna :: (String, SqlType, Nullability) -> IO ()
mostraColuna (nome, tipo, nulabilidade) =
  putStrLn ("  " ++ nome ++ " : " ++ show tipo ++ " " ++ show nulabilidade)
