-- | Parser compartilhado: tokenizador + analisador de descida recursiva para a
-- mini-linguagem SQL dos frontends estatico (modulo 5) e dinamico (modulo 6).
--
-- Este modulo e interno ao pacote. A gramatica aceita e:
--
-- > SELECT * FROM ident
-- > SELECT col1, col2 FROM ident
-- > SELECT * FROM ident WHERE col = "texto"
-- > SELECT * FROM ident WHERE col = numero
--
-- O resultado e uma 'Consulta' que pode ser usada tanto para gerar codigo
-- Template Haskell (frontend estatico) quanto para executar em runtime
-- (frontend dinamico).
module TypedQL.Frontend.Parser
  ( -- * Arvore sintatica
    Consulta (..)
  , Projecao (..)
  , Filtro (..)
  , Literal (..)
    -- * Execucao do parser
  , parseSql
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)

-- | Uma consulta analisada: o que projetar, de qual tabela, com qual filtro.
data Consulta = Consulta Projecao String Filtro
  deriving (Show)

-- | A clausula SELECT: ou todas as colunas, ou uma lista explicita.
data Projecao = Tudo | Colunas [String]
  deriving (Show)

-- | A clausula WHERE: ausente, ou uma igualdade coluna = literal.
data Filtro = SemFiltro | Igual String Literal
  deriving (Show)

-- | Os literais que o WHERE aceita.
data Literal = LitTexto String | LitInteiro Integer | LitFracionario Rational
  deriving (Show)

-- | Ponto de entrada: tokeniza e analisa numa so chamada.
parseSql :: String -> Either String Consulta
parseSql entrada = tokeniza entrada >>= analisa

-- ---------------------------------------------------------------------------
-- Tokenizador

data Token
  = TIdent String
  | TEstrela
  | TVirgula
  | TIgual
  | TTexto String
  | TNumero String
  deriving (Eq, Show)

tokeniza :: String -> Either String [Token]
tokeniza [] = Right []
tokeniza (c : cs)
  | isSpace c = tokeniza cs
  | c == '*'  = (TEstrela :) <$> tokeniza cs
  | c == ','  = (TVirgula :) <$> tokeniza cs
  | c == '='  = (TIgual :)   <$> tokeniza cs
  | c == '"'  =
      let (corpo, resto) = span (/= '"') cs
       in case resto of
            ('"' : resto') -> (TTexto corpo :) <$> tokeniza resto'
            _              -> Left "literal de texto sem aspas de fechamento."
  | isDigit c =
      let (numero, resto) = span (\x -> isDigit x || x == '.') (c : cs)
       in (TNumero numero :) <$> tokeniza resto
  | isAlpha c || c == '_' =
      let (ident, resto) = span (\x -> isAlphaNum x || x == '_') (c : cs)
       in (TIdent ident :) <$> tokeniza resto
  | otherwise = Left ("caractere inesperado: " ++ [c])

-- ---------------------------------------------------------------------------
-- Analisador (descida recursiva)

analisa :: [Token] -> Either String Consulta
analisa toks = do
  resto1         <- palavraChave "select" toks
  (proj, resto2) <- pProjecao resto1
  resto3         <- palavraChave "from" resto2
  (tab,  resto4) <- pIdent resto3
  (filt, resto5) <- pFiltro resto4
  case resto5 of
    []    -> Right (Consulta proj tab filt)
    sobra -> Left ("tokens sobrando no fim: " ++ show sobra)

palavraChave :: String -> [Token] -> Either String [Token]
palavraChave chave (TIdent nome : resto)
  | map toLower nome == chave = Right resto
palavraChave chave _ =
  Left ("esperava a palavra-chave " ++ map toUpperSimples chave ++ ".")

pProjecao :: [Token] -> Either String (Projecao, [Token])
pProjecao (TEstrela : resto) = Right (Tudo, resto)
pProjecao toks = do
  (cols, resto) <- pListaColunas toks
  Right (Colunas cols, resto)

pListaColunas :: [Token] -> Either String ([String], [Token])
pListaColunas toks = do
  (nome, resto) <- pIdent toks
  case resto of
    (TVirgula : resto') -> do
      (mais, resto'') <- pListaColunas resto'
      Right (nome : mais, resto'')
    _ -> Right ([nome], resto)

pIdent :: [Token] -> Either String (String, [Token])
pIdent (TIdent nome : resto) = Right (nome, resto)
pIdent _ = Left "esperava um identificador (nome de coluna ou de tabela)."

pFiltro :: [Token] -> Either String (Filtro, [Token])
pFiltro (TIdent kw : resto)
  | map toLower kw == "where" = do
      (coluna, resto1) <- pIdent resto
      resto2           <- casaIgual resto1
      (lit,   resto3)  <- pLiteral resto2
      Right (Igual coluna lit, resto3)
pFiltro toks = Right (SemFiltro, toks)

casaIgual :: [Token] -> Either String [Token]
casaIgual (TIgual : resto) = Right resto
casaIgual _ = Left "esperava = na condicao do WHERE."

pLiteral :: [Token] -> Either String (Literal, [Token])
pLiteral (TTexto  s : resto) = Right (LitTexto s, resto)
pLiteral (TNumero n : resto)
  | '.' `elem` n = Right (LitFracionario (toRational (read n :: Double)), resto)
  | otherwise    = Right (LitInteiro (read n), resto)
pLiteral _ = Left "esperava um literal (texto entre aspas ou numero)."

toUpperSimples :: Char -> Char
toUpperSimples c
  | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
  | otherwise            = c
