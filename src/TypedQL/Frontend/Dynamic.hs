{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Modulo 6: frontend dinamico. Executa SQL lido em runtime, devolvendo um
-- resultado cujo esquema e escondido num existencial.
--
-- Este modulo e o oposto do modulo 5. La, o SQL era uma literal no
-- codigo-fonte; o compilador o via, o quasiquoter o traduzia para
-- 'project'\/'select', e o GHC verificava o tipo do resultado. Aqui, a string
-- so existe em runtime -- pode vir de um arquivo, de uma entrada do usuario,
-- de um parametro de linha de comando. O tipo da consulta nao pode ser
-- determinado na compilacao: nos nao sabemos o esquema da tabela que sera
-- passada, nem as colunas que serao pedidas.
--
-- A solucao e o existencial. 'SomeTable' e 'SomeResult' escondem a variavel
-- de tipo @s@ (o esquema): dentro do construtor, @s@ existe e e coerente; de
-- fora, ela esta embrulhada e so pode ser acessada atraves dos eliminadores.
-- O singleton 'SSchema' e o que torna isso util: dentro do existencial, a
-- variavel de tipo e concreta, e o singleton carrega a informacao necessaria
-- para percorrer as linhas com 'header' e 'showRow'.
--
-- O constraint 'All Show s' tambem fica guardado no existencial. Isso e a chave:
-- sem ele, 'showRow' nao poderia ser chamado dentro do pattern match, porque o
-- GHC nao saberia que cada coluna de @s@ tem 'Show'. O existencial empacota nao
-- so o singleton, mas tambem a prova de que 'Show' vale para o esquema escondido.
--
-- O preco dessa flexibilidade e que erros de consulta (coluna inexistente,
-- tabela desconhecida) so podem ser detectados em runtime, como 'Left'. Isso
-- contrasta diretamente com o modulo 5, onde os mesmos erros sao rejeitados
-- pelo compilador. A garantia nao desaparece; ela muda de lugar: do tipo
-- para o 'Either'.
module TypedQL.Frontend.Dynamic
  ( -- * Existencial de tabela
    SomeTable (..)
  , withSomeTable
    -- * Resultado dinamico
  , SomeResult
  , withSomeResult
  , dynHeader
  , dynRows
  , dynRowCount
    -- * Execucao
  , runDynSQL
  ) where

import Data.List (find)
import TypedQL.Frontend.Parser
  ( Consulta (..)
  , Filtro (..)
  , Literal (..)
  , Projecao (..)
  , parseSql
  )
import TypedQL.Row (Row (..), SSchema (..), header, showRow)
import TypedQL.Schema (All, Nullability, SqlType)

-- ---------------------------------------------------------------------------
-- Existencial de tabela

-- | Uma tabela com esquema desconhecido em compile time. Alem do singleton
-- 'SSchema' e das linhas, o construtor empacota tambem a prova @All Show s@:
-- sem ela, nao seria possivel chamar 'showRow' depois de abrir o existencial.
--
-- A razao do singleton: precisamos de 'header' e 'showRow' sem conhecer @s@.
-- Se guardassemos so @[Row s]@, nao teriamos como percorrer as linhas depois
-- de abrir o existencial.
data SomeTable = forall s. All Show s => SomeTable (SSchema s) [Row s]

-- | Eliminador de 'SomeTable'. A continuacao recebe @s@ concretizado; dentro
-- dela e possivel usar 'header', 'showRow', e qualquer funcao do modulo 2.
-- Nao e possivel usar @col \@"vendor_code"@: o GHC nao sabe que @s@ e
-- 'Vendors'. Essa e a diferenca entre o existencial e o tipo estatico.
withSomeTable
  :: SomeTable
  -> (forall s. All Show s => SSchema s -> [Row s] -> r)
  -> r
withSomeTable (SomeTable sg rows) f = f sg rows

-- ---------------------------------------------------------------------------
-- Resultado dinamico

-- | O resultado de uma consulta dinamica. Guarda o esquema completo como
-- singleton (com prova @All Show s@), a lista de colunas selecionadas
-- (vazia = todas), e as linhas filtradas pelo WHERE.
--
-- A projecao do SELECT e aplicada na hora da observacao ('dynRows'). Isso
-- evita a necessidade de construir um novo singleton projetado em runtime,
-- o que exigiria manipulacao de listas de tipos promovidos que esta alem do
-- escopo deste modulo.
data SomeResult = forall s. All Show s => SomeResult (SSchema s) [String] [Row s]

-- | Eliminador de 'SomeResult'. Dentro da continuacao, @s@ e concretizado;
-- fora, esta escondido.
withSomeResult
  :: SomeResult
  -> (forall s. All Show s => SSchema s -> [Row s] -> r)
  -> r
withSomeResult (SomeResult sg _ rows) f = f sg rows

-- | Cabecalho do resultado: lista de @(nome, tipo, nulabilidade)@ para as
-- colunas selecionadas. Se nenhuma coluna foi explicitamente pedida, devolve
-- o esquema completo.
dynHeader :: SomeResult -> [(String, SqlType, Nullability)]
dynHeader (SomeResult sg cols _)
  | null cols = header sg
  | otherwise = filter (\(n, _, _) -> n `elem` cols) (header sg)

-- | Linhas do resultado como listas de strings. A projecao e aplicada aqui:
-- cada 'String' e o resultado de 'show' sobre o valor Haskell correspondente,
-- o mesmo formato que 'showRow' usa internamente.
dynRows :: SomeResult -> [[String]]
dynRows (SomeResult sg cols rows)
  | null cols = map (showRow sg) rows
  | otherwise = map (pegaPosicoes idxs . showRow sg) rows
  where
    allNames = map (\(n, _, _) -> n) (header sg)
    idxs     = [i | (i, n) <- zip [0..] allNames, n `elem` cols]

-- | Numero de linhas no resultado apos a aplicacao do WHERE.
dynRowCount :: SomeResult -> Int
dynRowCount (SomeResult _ _ rows) = length rows

pegaPosicoes :: [Int] -> [a] -> [a]
pegaPosicoes idxs xs = [xs !! i | i <- idxs]

-- ---------------------------------------------------------------------------
-- Execucao dinamica

-- | Executa uma string SQL contra um registro de tabelas.
--
-- O registro e uma lista de pares @(nome, SomeTable)@. Isso permite registrar
-- tabelas de esquemas diferentes sem precisar de um tipo unificado.
--
-- Erros possiveis (todos como @Left String@):
--
-- * Sintaxe invalida na string SQL.
-- * Tabela nao encontrada no registro.
-- * Coluna do SELECT inexistente na tabela.
-- * Coluna do WHERE inexistente na tabela.
--
-- Esses erros, que no modulo 5 eram erros de tipo, aqui sao @Left@ em runtime.
runDynSQL :: [(String, SomeTable)] -> String -> Either String SomeResult
runDynSQL registro entrada = do
  consulta <- parseSql entrada
  let Consulta proj tabelaNome filtro = consulta
  someTable <- buscaTabela registro tabelaNome
  aplicaConsulta someTable proj filtro

buscaTabela :: [(String, SomeTable)] -> String -> Either String SomeTable
buscaTabela reg nome =
  case lookup nome reg of
    Nothing -> Left ("tabela '" ++ nome ++ "' nao encontrada no registro.")
    Just t  -> Right t

-- | Abre o existencial e aplica as operacoes da consulta. Os erros de nome
-- de coluna so podem ser detectados aqui, em runtime.
aplicaConsulta :: SomeTable -> Projecao -> Filtro -> Either String SomeResult
aplicaConsulta (SomeTable sg rows) proj filtro = do
  colsSel <- validaProjecao sg proj
  mapM_ (validaColuna sg) (colunaDoFiltro filtro)
  let rowsFiltradas = applyDynFiltro sg filtro rows
  Right (SomeResult sg colsSel rowsFiltradas)

validaProjecao :: SSchema s -> Projecao -> Either String [String]
validaProjecao _  Tudo           = Right []
validaProjecao sg (Colunas cols) = mapM_ (validaColuna sg) cols >> Right cols

validaColuna :: SSchema s -> String -> Either String ()
validaColuna sg col =
  if col `elem` map (\(n, _, _) -> n) (header sg)
  then Right ()
  else Left ("coluna '" ++ col ++ "' nao existe na tabela.")

colunaDoFiltro :: Filtro -> [String]
colunaDoFiltro SemFiltro   = []
colunaDoFiltro (Igual c _) = [c]

-- | Aplica o filtro WHERE avaliando valores em runtime.
-- A comparacao usa 'showRow' + 'matchLit': ambos usam 'show', entao o formato
-- e consistente -- por exemplo, @show (T.pack "VFAKE") = "\"VFAKE\""@ e igual
-- a @show "VFAKE" :: String@.
applyDynFiltro :: All Show s => SSchema s -> Filtro -> [Row s] -> [Row s]
applyDynFiltro _  SemFiltro       rows = rows
applyDynFiltro sg (Igual col lit) rows =
  case colIdx sg col of
    Nothing  -> rows
    Just idx -> filter (\r -> matchLit (showRow sg r !! idx) lit) rows

-- | Indice zero-base de uma coluna no esquema.
colIdx :: SSchema s -> String -> Maybe Int
colIdx sg col =
  fmap fst $ find (\(_, (n, _, _)) -> n == col) (zip [0..] (header sg))

-- | Compara o valor renderizado de uma celula com o literal da clausula WHERE.
matchLit :: String -> Literal -> Bool
matchLit s (LitTexto      t) = s == show t
matchLit s (LitInteiro    n) = s == show n
matchLit s (LitFracionario r) = s == show (fromRational r :: Double)
