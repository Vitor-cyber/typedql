{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main (main) where

import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import TypedQL.Algebra
import TypedQL.Expr
import TypedQL.Frontend.Dynamic
import TypedQL.Frontend.Static (sql)
import TypedQL.Row
import TypedQL.Schema

-- | Esquema de teste. A ultima coluna aceita NULL: e ela que exercita o modulo 3.
type Vendors :: Schema
type Vendors =
  [ "vendor_code" := TText
  , "vendor_name" := TText
  , "open_rate" := TDouble
  , "defeitos" := TInt
  , "cnpj" :? TText
  ]

linha :: Row Vendors
linha =
  RCons "VFAKE" (RCons "Fornecedor Teste Um" (RCons 0.42 (RCons 17 (RCons (Just "00.000.000/0001-00") RNil))))

linhaSemCnpj :: Row Vendors
linhaSemCnpj =
  RCons "FAKEV" (RCons "Fornecedor Teste Dois" (RCons 0.10 (RCons 3 (RCons Nothing RNil))))

-- | Esquema de campanhas: disjunto de Vendors para poder ser juntado.
type Campanhas :: Schema
type Campanhas =
  [ "camp_id"     := TInt
  , "camp_vendor" := TText
  ]

-- Dados para os testes do modulo 4.
-- VFAKE tem 2 campanhas, FAKEV nao tem nenhuma.
tabelaVendors :: [Row Vendors]
tabelaVendors = [linha, linhaSemCnpj]

campanhas :: [Row Campanhas]
campanhas =
  [ RCons 1 (RCons "VFAKE" RNil)
  , RCons 2 (RCons "VFAKE" RNil)
  ]

-- Condicao de juncao: vendor_code = camp_vendor.
joinCond :: Predicate (Append Vendors Campanhas)
joinCond = EEq (colE @"vendor_code") (colE @"camp_vendor")

-- | Tabela em escopo, referenciada pelo quasiquoter em @FROM vendorsQ@.
-- O quasiquoter nao inventa a tabela: gera @VarE (mkName "vendorsQ")@, que
-- resolve para este binding.
vendorsQ :: Query Logical Vendors
vendorsQ = fromTable "vendors" tabelaVendors

-- Teste de nivel de tipos: Append junta esquemas na ordem certa.
appendJuntaEsquemas ::
  Proxy (Append '["a" := TInt] '["b" := TText]) ->
  Proxy '["a" := TInt, "b" := TText]
appendJuntaEsquemas = id

-- Testes no nivel de tipos: se estas definicoes compilam, a propriedade vale.
-- Cada uma e uma igualdade de tipos verificada pelo GHC, nada roda em runtime.
tipoDaColuna :: Proxy (TypeOf "open_rate" Vendors) -> Proxy TDouble
tipoDaColuna = id

nomesDoEsquema ::
  Proxy (Names Vendors) ->
  Proxy ["vendor_code", "vendor_name", "open_rate", "defeitos", "cnpj"]
nomesDoEsquema = id

projecaoPreservaTipo ::
  Proxy (Project '["open_rate"] Vendors) -> Proxy '["open_rate" := TDouble]
projecaoPreservaTipo = id

projecaoPreservaNulabilidade ::
  Proxy (Project '["cnpj"] Vendors) -> Proxy '["cnpj" :? TText]
projecaoPreservaNulabilidade = id

renomeiaPreservaTipo ::
  Proxy (Rename "open_rate" "taxa" Vendors) ->
  Proxy
    [ "vendor_code" := TText
    , "vendor_name" := TText
    , "taxa" := TDouble
    , "defeitos" := TInt
    , "cnpj" :? TText
    ]
renomeiaPreservaTipo = id

interpEInjetiva :: Proxy (Interp TInt) -> Proxy Int
interpEInjetiva = id

nulabilidadeDaColuna :: Proxy (NullabilityOf "cnpj" Vendors) -> Proxy Nullable
nulabilidadeDaColuna = id

slotObrigatorioNaoTemMaybe :: Proxy (Slot ("open_rate" := TDouble)) -> Proxy Double
slotObrigatorioNaoTemMaybe = id

slotNulavelTemMaybe :: Proxy (Slot ("cnpj" :? TText)) -> Proxy (Maybe Text)
slotNulavelTemMaybe = id

todasNulaveis ::
  Proxy (MakeNullable '["defeitos" := TInt, "cnpj" :? TText]) ->
  Proxy ["defeitos" :? TInt, "cnpj" :? TText]
todasNulaveis = id

mergeContamina :: Proxy (MergeNull NotNull Nullable) -> Proxy Nullable
mergeContamina = id

mergePreservaTotal :: Proxy (MergeNull NotNull NotNull) -> Proxy NotNull
mergePreservaTotal = id

-- | Predicado de topo, com assinatura. @renderExpr@ nao menciona o esquema no
-- resultado, logo o GHC nao consegue inferir @s@ a partir do uso: a anotacao e
-- obrigatoria. Note que o predicado e total (@NotNull@) mesmo citando uma coluna
-- nulavel, porque @IS NULL@ e a valvula de escape da nulabilidade.
predicadoImpresso :: Predicate Vendors
predicadoImpresso =
  EAnd
    (EIsNull (colE @"cnpj"))
    (ELt (colE @"open_rate") (ELit 0.5))

main :: IO ()
main = defaultMain $ testGroup "TypedQL"
  [ testGroup "Schema, nivel de tipos"
      [ testCase "TypeOf devolve o tipo da coluna" $ const () (tipoDaColuna Proxy) @?= ()
      , testCase "Names lista os nomes na ordem" $ const () (nomesDoEsquema Proxy) @?= ()
      , testCase "Project preserva o tipo da coluna" $ const () (projecaoPreservaTipo Proxy) @?= ()
      , testCase "Project preserva a nulabilidade" $ const () (projecaoPreservaNulabilidade Proxy) @?= ()
      , testCase "Rename troca o nome e mantem o resto" $ const () (renomeiaPreservaTipo Proxy) @?= ()
      , testCase "Interp e injetiva" $ const () (interpEInjetiva Proxy) @?= ()
      , testCase "NullabilityOf le a nulabilidade" $ const () (nulabilidadeDaColuna Proxy) @?= ()
      , testCase "Slot obrigatorio nao embrulha em Maybe" $ const () (slotObrigatorioNaoTemMaybe Proxy) @?= ()
      , testCase "Slot nulavel embrulha em Maybe" $ const () (slotNulavelTemMaybe Proxy) @?= ()
      , testCase "MakeNullable afrouxa o esquema todo" $ const () (todasNulaveis Proxy) @?= ()
      , testCase "MergeNull contamina se um lado e nulavel" $ const () (mergeContamina Proxy) @?= ()
      , testCase "MergeNull preserva total se os dois sao totais" $ const () (mergePreservaTotal Proxy) @?= ()
      ]
  , testGroup "Schema, nivel de valores"
      [ testCase "demote e a inversa do singleton" $
          demote STInt @?= TInt
      , testCase "demoteNull idem para nulabilidade" $
          demoteNull SNullable @?= Nullable
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
      , testCase "col devolve Int na coluna inteira" $
          col @"defeitos" linha @?= 17
      , testCase "col devolve Maybe na coluna nulavel" $
          col @"cnpj" linha @?= Just "00.000.000/0001-00"
      , testCase "col devolve Nothing quando a coluna nulavel esta vazia" $
          col @"cnpj" linhaSemCnpj @?= Nothing
      , testCase "header reflete o esquema inteiro" $
          header (schemaSing @Vendors)
            @?= [ ("vendor_code", TText, NotNull)
                , ("vendor_name", TText, NotNull)
                , ("open_rate", TDouble, NotNull)
                , ("defeitos", TInt, NotNull)
                , ("cnpj", TText, Nullable)
                ]
      , testCase "showRow percorre a linha usando All Show" $
          showRow (schemaSing @Vendors) linha
            @?= [ "\"VFAKE\""
                , "\"Fornecedor Teste Um\""
                , "0.42"
                , "17"
                , "Just \"00.000.000/0001-00\""
                ]
      , testCase "withRow elimina o existencial de linha" $
          withRow (SomeRow (schemaSing @Vendors) linha) (\s _ -> length (header s)) @?= 5
      ]
  , testGroup "Expr, avaliacao"
      [ testCase "coluna obrigatoria avalia para o valor cru" $
          evalExpr linha (colE @"open_rate") @?= 0.42
      , testCase "coluna nulavel avalia para Maybe" $
          evalExpr linha (colE @"cnpj") @?= Just "00.000.000/0001-00"
      , testCase "soma de dois totais nao passa por Maybe" $
          evalExpr linha (EAdd (colE @"defeitos") (ELit 3)) @?= 20
      , testCase "NULL contamina a soma" $
          evalExpr linha (EAdd (colE @"defeitos") (ENull STInt)) @?= Nothing
      , testCase "igualdade com NULL da NULL, nao False" $
          evalExpr linhaSemCnpj (EEq (colE @"cnpj") (ELit "00.000.000/0001-00")) @?= Nothing
      , testCase "igualdade entre totais decide" $
          evalWhere linha (EEq (colE @"vendor_code") (ELit "VFAKE")) @?= True
      , testCase "IS NULL sempre decide" $
          evalWhere linhaSemCnpj (EIsNull (colE @"cnpj")) @?= True
      , testCase "COALESCE devolve valor cru, sem Maybe" $
          evalExpr linhaSemCnpj (ECoalesce (colE @"cnpj") (ELit "sem cnpj")) @?= "sem cnpj"
      , testCase "COALESCE deixa passar o valor presente" $
          evalExpr linha (ECoalesce (colE @"cnpj") (ELit "sem cnpj")) @?= "00.000.000/0001-00"
      ]
  , testGroup "Expr, logica de tres valores"
      [ testCase "FALSE AND NULL e FALSE, nao NULL" $
          evalExpr linhaSemCnpj (EAnd (ELit False) (EEq (colE @"cnpj") (ELit "x"))) @?= Just False
      , testCase "TRUE OR NULL e TRUE, nao NULL" $
          evalExpr linhaSemCnpj (EOr (ELit True) (EEq (colE @"cnpj") (ELit "x"))) @?= Just True
      , testCase "TRUE AND NULL continua NULL" $
          evalExpr linhaSemCnpj (EAnd (ELit True) (EEq (colE @"cnpj") (ELit "x"))) @?= Nothing
      , testCase "kleeneAnd e comutativo no caso FALSE" $
          kleeneAnd Nothing (Just False) @?= Just False
      , testCase "NOT preserva a nulabilidade" $
          evalExpr linhaSemCnpj (ENot (EEq (colE @"cnpj") (ELit "x"))) @?= Nothing
      , testCase "predicado total decide na linha sem cnpj" $
          evalWhere linhaSemCnpj predicadoImpresso @?= True
      , testCase "predicado total decide na linha com cnpj" $
          evalWhere linha predicadoImpresso @?= False
      ]
  , testGroup "Expr, impressao"
      [ testCase "renderExpr reconstroi o SQL" $
          renderExpr predicadoImpresso
            @?= "((cnpj IS NULL) AND (open_rate < 0.5))"
      , testCase "renderExpr mostra o tipo do NULL" $
          renderExpr (ENull STInt :: Expr Vendors TInt Nullable) @?= "NULL::TInt"
      ]
  , testGroup "Algebra"
      [ testCase "Append junta esquemas corretamente" $
          const () (appendJuntaEsquemas Proxy) @?= ()
      , testCase "fromTable + compile + eval devolve todas as linhas" $
          length (eval (compile (fromTable "v" tabelaVendors))) @?= 2
      , testCase "select filtra as linhas que nao passam no predicado" $
          let filtroCnpjNull :: Predicate Vendors
              filtroCnpjNull = EIsNull (colE @"cnpj")
          in length (eval (compile (select filtroCnpjNull (fromTable "v" tabelaVendors)))) @?= 1
      , testCase "project reduz o numero de colunas" $
          let q = project (Proxy @'["vendor_code"]) (fromTable "v" tabelaVendors)
              rows = eval (compile q)
          in length rows @?= 2
      , testCase "project preserva os valores das colunas selecionadas" $
          let q = project (Proxy @'["vendor_code"]) (fromTable "v" tabelaVendors)
              rows = eval (compile q)
          in map (col @"vendor_code") rows @?= ["VFAKE", "FAKEV"]
      , testCase "innerJoin descarta o lado sem correspondencia" $
          length (eval (compile (innerJoin joinCond (fromTable "v" tabelaVendors) (fromTable "c" campanhas)))) @?= 2
      , testCase "leftJoin mantem todas as linhas do lado esquerdo" $
          length (eval (compile (leftJoin joinCond (fromTable "v" tabelaVendors) (fromTable "c" campanhas)))) @?= 3
      , testCase "leftJoin preenche com Nothing quando nao ha correspondencia" $
          let rows = eval (compile (leftJoin joinCond (fromTable "v" tabelaVendors) (fromTable "c" campanhas)))
              semCamp = filter (\r -> col @"vendor_code" r == "FAKEV") rows
          in map (col @"camp_id") semCamp @?= [Nothing]
      , testCase "renderSQL produz SQL com WHERE" $
          let q = select (EIsNull (colE @"cnpj" :: Expr Vendors TText Nullable)) (fromTable "vendors" tabelaVendors)
          in renderSQL q @?= "SELECT * FROM (vendors) WHERE (cnpj IS NULL)"
      ]
  , testGroup "Frontend.Static (quasiquoter SQL)"
      [ testCase "SELECT * traz todas as linhas" $
          length (eval (compile [sql| SELECT * FROM vendorsQ |])) @?= 2
      , testCase "SELECT lista projeta as colunas pedidas" $
          map (col @"vendor_code") (eval (compile [sql| SELECT vendor_code FROM vendorsQ |]))
            @?= ["VFAKE", "FAKEV"]
      , testCase "WHERE com literal de texto filtra" $
          map (col @"vendor_code") (eval (compile [sql| SELECT vendor_code FROM vendorsQ WHERE vendor_code = "VFAKE" |]))
            @?= ["VFAKE"]
      , testCase "WHERE com literal numerico filtra" $
          map (col @"vendor_code") (eval (compile [sql| SELECT vendor_code FROM vendorsQ WHERE defeitos = 17 |]))
            @?= ["VFAKE"]
      , testCase "SQL equivale a algebra escrita a mao" $
          map (col @"vendor_code") (eval (compile [sql| SELECT vendor_code FROM vendorsQ WHERE vendor_code = "VFAKE" |]))
            @?= map (col @"vendor_code")
                  (eval (compile (project (Proxy @'["vendor_code"])
                                          (select (EEq (colE @"vendor_code") (ELit "VFAKE")) vendorsQ))))
      , testCase "renderSQL do plano gerado tem WHERE e projecao" $
          renderSQL [sql| SELECT vendor_code FROM vendorsQ WHERE vendor_code = "VFAKE" |]
            @?= "SELECT vendor_code FROM (SELECT * FROM (vendors) WHERE (vendor_code = \"VFAKE\"))"
      ]
  , testGroup "Frontend.Dynamic (existencial + singleton)"
      [ testCase "SELECT * devolve todas as linhas" $
          fmap dynRowCount (runDynSQL registro "SELECT * FROM vendors") @?= Right 2
      , testCase "dynHeader reflete o esquema completo no SELECT *" $
          fmap (map (\(n,_,_)->n) . dynHeader) (runDynSQL registro "SELECT * FROM vendors")
            @?= Right ["vendor_code", "open_rate"]
      , testCase "SELECT lista projeta colunas na observacao" $
          fmap dynRows (runDynSQL registro "SELECT vendor_code FROM vendors")
            @?= Right [["\"VFAKE\""], ["\"FAKEV\""]]
      , testCase "WHERE filtra linhas em runtime" $
          fmap dynRowCount (runDynSQL registro "SELECT * FROM vendors WHERE vendor_code = \"VFAKE\"")
            @?= Right 1
      , testCase "dynHeader com SELECT lista mostra so as colunas pedidas" $
          fmap (map (\(n,_,_)->n) . dynHeader)
               (runDynSQL registro "SELECT vendor_code FROM vendors")
            @?= Right ["vendor_code"]
      , testCase "tabela nao encontrada devolve Left" $
          case runDynSQL registro "SELECT * FROM inexistente" of
            Left msg -> assertBool "mensagem menciona tabela" ("inexistente" `isInfixOf` msg)
            Right _  -> assertFailure "deveria ter falhado"
      , testCase "coluna inexistente no SELECT devolve Left" $
          case runDynSQL registro "SELECT taxa FROM vendors" of
            Left msg -> assertBool "mensagem menciona coluna" ("taxa" `isInfixOf` msg)
            Right _  -> assertFailure "deveria ter falhado"
      , testCase "withSomeTable permite observar o esquema sem conhecer s" $
          let tbl = SomeTable (schemaSing @DynVendors) []
          in withSomeTable tbl (\sg _ -> length (header sg)) @?= 2
      ]
  ]

-- Registro de tabelas para os testes do modulo 6.
-- SomeTable empacota a tabela com seu singleton.
type DynVendors :: Schema
type DynVendors = ["vendor_code" := TText, "open_rate" := TDouble]

registro :: [(String, SomeTable)]
registro =
  [ ( "vendors"
    , SomeTable
        (schemaSing @DynVendors)
        [ RCons "VFAKE" (RCons 0.42 RNil)
        , RCons "FAKEV" (RCons 0.08 RNil)
        ]
    )
  ]
