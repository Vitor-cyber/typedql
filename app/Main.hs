{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import TypedQL.Algebra
import TypedQL.Expr
import TypedQL.Frontend.Dynamic
import TypedQL.Frontend.Static (sql)
import TypedQL.Engine
import TypedQL.Optimize
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

-- | Um filtro escrito sobre o esquema da juncao, para o modulo 7 absorver na
-- condicao do INNER JOIN.
filtroPosJuncao :: Predicate (Append Vendors Campanhas)
filtroPosJuncao = ELt (ELit 0) (colE @"camp_id")

main :: IO ()
main = do
  putStrLn "TypedQL 0.1 - modulos 1 a 8"
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
  putStrLn ""

  putStrLn "--- modulo 5: Frontend.Static ---"
  putStrLn ""
  putStrLn "O quasiquoter le SQL de verdade e gera a consulta tipada em compile time."
  putStrLn "O que voce escreve como texto vira as mesmas chamadas a project/select/colE,"
  putStrLn "e o GHC verifica tudo. SQL com coluna inexistente nao compila."
  putStrLn ""
  let vendorsQ = fromTable "vendors" tabelaVendors
      planoSql = [sql| SELECT vendor_code, open_rate FROM vendorsQ WHERE vendor_code = "VFAKE" |]
  putStrLn "  [sql| SELECT vendor_code, open_rate FROM vendorsQ WHERE vendor_code = 'VFAKE' |]"
  putStrLn ("  vira (SQL): " ++ renderSQL planoSql)
  let linhasSql = eval (compile planoSql)
  putStrLn ("  Resultado (" ++ show (length linhasSql) ++ " linhas):")
  mapM_ (\r -> putStrLn ("    vendor=" ++ T.unpack (col @"vendor_code" r) ++ ", open_rate=" ++ show (col @"open_rate" r))) linhasSql
  putStrLn ""
  putStrLn "Filtro numerico, mesma sintaxe:"
  let planoNum = [sql| SELECT vendor_code FROM vendorsQ WHERE defeitos = 17 |]
  putStrLn ("  [sql| SELECT vendor_code FROM vendorsQ WHERE defeitos = 17 |]")
  putStrLn ("  vira (SQL): " ++ renderSQL planoNum)
  putStrLn ("  Resultado: " ++ show (map (T.unpack . col @"vendor_code") (eval (compile planoNum))))

  putStrLn ""
  putStrLn "--- modulo 6: Frontend.Dynamic ---"
  putStrLn ""
  putStrLn "O mesmo SQL, mas como string de runtime. O tipo do resultado e desconhecido"
  putStrLn "em compile time; erros de nome de coluna so aparecem em runtime, como Left."
  putStrLn ""
  let simples = SomeTable (schemaSing @Vendors) tabelaVendors
      reg = [("vendors", simples)]
  case runDynSQL reg "SELECT vendor_code, open_rate FROM vendors WHERE defeitos = 7" of
    Left err -> putStrLn ("Erro: " ++ err)
    Right res -> do
      putStrLn "  SQL: SELECT vendor_code, open_rate FROM vendors WHERE defeitos = 7"
      putStrLn ("  Cabecalho: " ++ show (map (\(n,_,_)->n) (dynHeader res)))
      putStrLn ("  Linhas (" ++ show (dynRowCount res) ++ "):")
      mapM_ (\row -> putStrLn ("    " ++ show row)) (dynRows res)
  putStrLn ""
  case runDynSQL reg "SELECT taxa FROM vendors" of
    Left err -> putStrLn ("Coluna inexistente -> Left: " ++ err)
    Right _  -> putStrLn "ERRO: deveria ter falhado"

  putStrLn ""
  putStrLn "--- modulo 7: Optimize ---"
  putStrLn ""
  putStrLn "O otimizador e um catamorfismo sobre a arvore de consulta. O tipo de"
  putStrLn "compileWith obriga a reescrita a devolver um plano sobre o MESMO esquema,"
  putStrLn "entao um otimizador que muda a forma do resultado nao compila."
  putStrLn ""
  let planoIngenuo =
        select (ELit True) (select filtroCritico (select filtroCritico (fromTable "vendors" tabelaVendors)))
      fisicoIngenuo = compileOtimizado planoIngenuo
  putStrLn ("Logico  (" ++ show (tamanhoPlano planoIngenuo) ++ " nos): " ++ renderSQL planoIngenuo)
  putStrLn ("Fisico  (" ++ show (tamanhoPlano fisicoIngenuo) ++ " nos): " ++ renderSQL fisicoIngenuo)
  mapM_ (\r -> putStrLn ("  reescrita: " ++ r)) (explicar planoIngenuo)
  putStrLn
    ( "  mesmo resultado que sem otimizar: "
        ++ show (length (eval fisicoIngenuo) == length (eval (compile planoIngenuo)))
    )
  putStrLn ""
  putStrLn "Filtro sobre INNER JOIN vira condicao de juncao (a reescrita classica):"
  let planoComFiltro =
        select filtroPosJuncao
          (innerJoin joinCond (fromTable "vendors" tabelaVendors) (fromTable "campanhas" campanhas))
      fisicoComFiltro = compileOtimizado planoComFiltro
  putStrLn ("  Logico: " ++ renderSQL planoComFiltro)
  putStrLn ("  Fisico: " ++ renderSQL fisicoComFiltro)
  mapM_ (\r -> putStrLn ("  reescrita: " ++ r)) (explicar planoComFiltro)
  putStrLn ""
  putStrLn "Filtro impossivel curto-circuita a subarvore inteira:"
  let planoImpossivel = select (ELit False) (fromTable "vendors" tabelaVendors)
  putStrLn ("  Fisico: " ++ renderSQL (compileOtimizado planoImpossivel))
  putStrLn ("  Linhas lidas: " ++ show (length (eval (compileOtimizado planoImpossivel))))
  putStrLn ""
  putStrLn "A reescrita que o tipo PROIBE: absorver o WHERE no ON de um LEFT JOIN."
  putStrLn "Em SQL isso muda a resposta; aqui o predicado do LEFT JOIN tem tipo"
  putStrLn "Predicate (Append l r) e o filtro de cima tem Predicate (Append l (MakeNullable r))."
  putStrLn "Ver negativos/AbsorcaoEmLeftJoin.hs."

  putStrLn ""
  putStrLn "--- modulo 8: Engine ---"
  putStrLn ""
  putStrLn "Operadores fisicos indexados pelo esquema que produzem. O EXPLAIN mostra"
  putStrLn "o algoritmo escolhido, nao o SQL equivalente."
  putStrLn ""
  let planoParaExecutar =
        select filtroPosJuncao
          (innerJoin joinCond (fromTable "vendors" tabelaVendors) (fromTable "campanhas" campanhas))
  putStr (explainPlano (compileOtimizado planoParaExecutar))
  putStrLn ""
  putStrLn ""
  putStrLn "O mesmo resultado por hash join, pedido explicitamente. As chaves vao por"
  putStrLn "aplicacao de tipo, e o construtor exige que as duas sejam NotNull e do"
  putStrLn "mesmo tipo SQL:"
  putStrLn ""
  -- Tabelas replicadas: com 2x2 linhas o produto e a soma empatam (2*2 = 2+2) e a
  -- diferenca entre os dois algoritmos nao aparece.
  let vendorsMuitos = concat (replicate 25 tabelaVendors)
      campanhasMuitas = concat (replicate 25 campanhas)
      opLaco = NLJoin joinCond (scan "vendors" vendorsMuitos) (scan "campanhas" campanhasMuitas)
      opHash = hashJoin @"vendor_code" @"camp_vendor" (scan "vendors" vendorsMuitos) (scan "campanhas" campanhasMuitas)
  putStr (explainOp opHash)
  putStrLn ""
  putStrLn ("  laco aninhado: " ++ show (estimar opLaco))
  putStrLn ("  hash join:     " ++ show (estimar opHash))
  putStrLn
    ( "  mesmo resultado: "
        ++ show (map (col @"camp_id") (runOp opHash) == map (col @"camp_id") (runOp opLaco))
    )
  putStrLn ""
  putStrLn "Por que o hash join precisa de chave NotNull: em SQL, NULL = NULL nao e"
  putStrLn "verdadeiro, entao linha de chave NULL nunca casa. Um hash join ingenuo"
  putStrLn "coloca o NULL no balde e devolve linhas que nao existem. Aqui o operador"
  putStrLn "com chave nulavel nao pode ser construido: negativos/HashJoinChaveNulavel.hs."

mostraColuna :: (String, SqlType, Nullability) -> IO ()
mostraColuna (nome, tipo, nulabilidade) =
  putStrLn ("  " ++ nome ++ " : " ++ show tipo ++ " " ++ show nulabilidade)
