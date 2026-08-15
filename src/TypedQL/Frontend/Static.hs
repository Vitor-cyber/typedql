{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Modulo 5: frontend estatico. Um quasiquoter que aceita SQL escrito a mao e o
-- transforma, /em tempo de compilacao/, numa consulta tipada dos modulos 1 a 4.
--
-- A pergunta deste modulo: e possivel escrever SQL de verdade, com a sintaxe
-- familiar @SELECT ... FROM ... WHERE ...@, e ainda assim ter todas as garantias
-- de tipo dos modulos anteriores? A resposta e o Template Haskell. O quasiquoter
-- roda antes da compilacao propriamente dita: ele le a string, analisa a
-- gramatica, e /gera codigo Haskell/ -- as mesmas chamadas a 'project', 'select',
-- 'colE' que voce escreveria a mao. O GHC entao verifica esse codigo gerado
-- exatamente como verificaria o codigo escrito por voce.
--
-- Consequencia importante: o quasiquoter nao verifica tipos. Ele so traduz
-- sintaxe. Toda a checagem continua sendo feita pelo GHC sobre o codigo gerado.
-- Se o SQL menciona uma coluna que nao existe, o quasiquoter gera um
-- @project (Proxy \@'["taxa"]) ...@ perfeitamente bem-formado, e e o GHC que
-- rejeita, com a mesma mensagem do modulo 2. O erro de nome de coluna, que num
-- banco tradicional so aparece quando a query roda, aqui aparece ao compilar.
--
-- Isto e o lado /estatico/ do frontend: a consulta e conhecida na hora da
-- compilacao. O modulo 6 fara o lado dinamico, onde a string so existe em runtime
-- e o tipo precisa ser reconstruido com singletons e existenciais.
--
-- Sintaxe aceita (subconjunto proposital):
--
-- > [sql| SELECT * FROM vendors |]
-- > [sql| SELECT vendor_code, open_rate FROM vendors |]
-- > [sql| SELECT vendor_code FROM vendors WHERE vendor_code = "AMZN" |]
-- > [sql| SELECT vendor_name FROM vendors WHERE defeitos = 3 |]
--
-- O nome depois de FROM e um identificador Haskell em escopo: uma
-- @Query st esquema@ (ou o resultado de 'fromTable'). O quasiquoter nao inventa a
-- tabela, ele referencia a que voce ja definiu.
module TypedQL.Frontend.Static
  ( sql
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Language.Haskell.TH
  ( Exp (..)
  , Lit (..)
  , Q
  , Type (..)
  , TyLit (..)
  , mkName
  )
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import TypedQL.Algebra (project, select)
import TypedQL.Expr (Expr (..), colE)

-- ---------------------------------------------------------------------------
-- O quasiquoter
-- ---------------------------------------------------------------------------

-- | O quasiquoter de SQL estatico. So vale em posicao de expressao: nao ha
-- sentido em ter um padrao ou um tipo escrito como SQL.
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = compilaSql
    , quotePat = const (fail erroPosicao)
    , quoteType = const (fail erroPosicao)
    , quoteDec = const (fail erroPosicao)
    }
  where
    erroPosicao =
      "TypedQL/SQL: [sql| ... |] so pode aparecer em posicao de expressao."

-- | O caminho completo: string -> tokens -> arvore -> Exp do Template Haskell.
-- Um erro de gramatica vira um erro de compilacao com 'fail'; um erro de /tipo/
-- (coluna inexistente, tipo incompativel) so aparece depois, quando o GHC
-- verifica a Exp gerada.
compilaSql :: String -> Q Exp
compilaSql entrada =
  case tokeniza entrada >>= analisa of
    Left erro -> fail ("TypedQL/SQL: " ++ erro)
    Right consulta -> pure (geraExp consulta)

-- ---------------------------------------------------------------------------
-- Arvore sintatica
-- ---------------------------------------------------------------------------

-- | Uma consulta analisada: o que projetar, de qual tabela, com qual filtro.
data Consulta = Consulta Projecao String Filtro

-- | A clausula SELECT: ou todas as colunas, ou uma lista explicita.
data Projecao = Tudo | Colunas [String]

-- | A clausula WHERE: ausente, ou uma igualdade coluna = literal.
data Filtro = SemFiltro | Igual String Literal

-- | Os literais que o WHERE aceita. Texto vira 'T.pack', inteiro e fracionario
-- ficam polimorficos para o GHC unificar com o tipo da coluna.
data Literal = LitTexto String | LitInteiro Integer | LitFracionario Rational

-- ---------------------------------------------------------------------------
-- Tokenizador
-- ---------------------------------------------------------------------------

-- | Os tokens da mini-linguagem.
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
  | c == '*' = (TEstrela :) <$> tokeniza cs
  | c == ',' = (TVirgula :) <$> tokeniza cs
  | c == '=' = (TIgual :) <$> tokeniza cs
  | c == '"' =
      let (corpo, resto) = span (/= '"') cs
       in case resto of
            ('"' : resto') -> (TTexto corpo :) <$> tokeniza resto'
            _ -> Left "literal de texto sem aspas de fechamento."
  | isDigit c =
      let (numero, resto) = span (\x -> isDigit x || x == '.') (c : cs)
       in (TNumero numero :) <$> tokeniza resto
  | isAlpha c || c == '_' =
      let (ident, resto) = span (\x -> isAlphaNum x || x == '_') (c : cs)
       in (TIdent ident :) <$> tokeniza resto
  | otherwise = Left ("caractere inesperado: " ++ [c])

-- ---------------------------------------------------------------------------
-- Analisador (descida recursiva)
-- ---------------------------------------------------------------------------

analisa :: [Token] -> Either String Consulta
analisa toks = do
  resto1 <- palavraChave "select" toks
  (proj, resto2) <- pProjecao resto1
  resto3 <- palavraChave "from" resto2
  (tabela, resto4) <- pIdent resto3
  (filtro, resto5) <- pFiltro resto4
  case resto5 of
    [] -> Right (Consulta proj tabela filtro)
    sobra -> Left ("tokens sobrando no fim: " ++ show sobra)

-- | Consome um identificador esperado como palavra-chave, sem diferenciar
-- maiusculas de minusculas.
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
      resto2 <- casaIgual resto1
      (lit, resto3) <- pLiteral resto2
      Right (Igual coluna lit, resto3)
pFiltro toks = Right (SemFiltro, toks)

casaIgual :: [Token] -> Either String [Token]
casaIgual (TIgual : resto) = Right resto
casaIgual _ = Left "esperava = na condicao do WHERE."

pLiteral :: [Token] -> Either String (Literal, [Token])
pLiteral (TTexto s : resto) = Right (LitTexto s, resto)
pLiteral (TNumero n : resto)
  | '.' `elem` n = Right (LitFracionario (toRational (read n :: Double)), resto)
  | otherwise = Right (LitInteiro (read n), resto)
pLiteral _ = Left "esperava um literal (texto entre aspas ou numero)."

toUpperSimples :: Char -> Char
toUpperSimples c
  | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
  | otherwise = c

-- ---------------------------------------------------------------------------
-- Geracao de codigo
-- ---------------------------------------------------------------------------

-- | Transforma a arvore analisada na Exp que o GHC vai verificar.
-- A ordem espelha a semantica do SQL: primeiro a tabela, depois o WHERE
-- (selecao), por fim o SELECT (projecao).
geraExp :: Consulta -> Exp
geraExp (Consulta proj tabela filtro) =
  aplicaProjecao proj (aplicaFiltro filtro tabelaExp)
  where
    tabelaExp = VarE (mkName tabela)

aplicaFiltro :: Filtro -> Exp -> Exp
aplicaFiltro SemFiltro alvo = alvo
aplicaFiltro (Igual coluna lit) alvo =
  AppE (AppE (VarE 'select) (geraPredicado coluna lit)) alvo

aplicaProjecao :: Projecao -> Exp -> Exp
aplicaProjecao Tudo alvo = alvo
aplicaProjecao (Colunas cols) alvo =
  AppE (AppE (VarE 'project) (geraProxy cols)) alvo

-- | Gera @(Proxy :: Proxy '["c1", "c2", ...])@. E a ponte entre a lista de
-- strings do parser e a lista de simbolos no nivel de tipos que 'project' exige.
geraProxy :: [String] -> Exp
geraProxy cols = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (listaTipo cols))
  where
    listaTipo =
      foldr (\c acc -> AppT (AppT PromotedConsT (LitT (StrTyLit c))) acc) PromotedNilT

-- | Gera @EEq (colE \@"coluna") (ELit literal)@. Se a coluna for nulavel, o tipo
-- do resultado sera @Expr s TBool Nullable@ e 'select' vai recusar: o filtro do
-- WHERE tem que decidir. Isso e checagem do GHC, nao do quasiquoter.
geraPredicado :: String -> Literal -> Exp
geraPredicado coluna lit =
  AppE (AppE (ConE 'EEq) (geraColuna coluna)) (geraLiteral lit)

-- | Gera @colE \@"coluna"@. A aplicacao de tipo carrega o nome da coluna para o
-- nivel de tipos; a prova de pertinencia e resolvida pelo GHC no uso.
geraColuna :: String -> Exp
geraColuna coluna = AppTypeE (VarE 'colE) (LitT (StrTyLit coluna))

geraLiteral :: Literal -> Exp
geraLiteral (LitTexto s) =
  AppE (ConE 'ELit) (AppE (VarE 'T.pack) (LitE (StringL s)))
geraLiteral (LitInteiro n) =
  AppE (ConE 'ELit) (LitE (IntegerL n))
geraLiteral (LitFracionario r) =
  AppE (ConE 'ELit) (LitE (RationalL r))
