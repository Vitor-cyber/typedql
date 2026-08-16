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
import TypedQL.Frontend.Parser
  ( Consulta (..)
  , Filtro (..)
  , Literal (..)
  , Projecao (..)
  , parseSql
  )

-- ---------------------------------------------------------------------------
-- O quasiquoter

-- | O quasiquoter de SQL estatico. So vale em posicao de expressao: nao ha
-- sentido em ter um padrao ou um tipo escrito como SQL.
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp  = compilaSql
    , quotePat  = const (fail erroPosicao)
    , quoteType = const (fail erroPosicao)
    , quoteDec  = const (fail erroPosicao)
    }
  where
    erroPosicao =
      "TypedQL/SQL: [sql| ... |] so pode aparecer em posicao de expressao."

-- | O caminho completo: string -> arvore -> Exp do Template Haskell.
-- Um erro de gramatica vira um erro de compilacao com 'fail'; um erro de /tipo/
-- (coluna inexistente, tipo incompativel) so aparece depois, quando o GHC
-- verifica a Exp gerada.
compilaSql :: String -> Q Exp
compilaSql entrada =
  case parseSql entrada of
    Left  erro     -> fail ("TypedQL/SQL: " ++ erro)
    Right consulta -> pure (geraExp consulta)

-- ---------------------------------------------------------------------------
-- Geracao de codigo

-- | Transforma a arvore analisada na Exp que o GHC vai verificar.
-- A ordem espelha a semantica do SQL: primeiro a tabela, depois o WHERE
-- (selecao), por fim o SELECT (projecao).
geraExp :: Consulta -> Exp
geraExp (Consulta proj tabela filtro) =
  aplicaProjecao proj (aplicaFiltro filtro tabelaExp)
  where
    tabelaExp = VarE (mkName tabela)

aplicaFiltro :: Filtro -> Exp -> Exp
aplicaFiltro SemFiltro     alvo = alvo
aplicaFiltro (Igual c lit) alvo =
  AppE (AppE (VarE 'select) (geraPredicado c lit)) alvo

aplicaProjecao :: Projecao -> Exp -> Exp
aplicaProjecao Tudo           alvo = alvo
aplicaProjecao (Colunas cols) alvo =
  AppE (AppE (VarE 'project) (geraProxy cols)) alvo

-- | Gera @(Proxy :: Proxy '["c1", "c2", ...])@. E a ponte entre a lista de
-- strings do parser e a lista de simbolos no nivel de tipos que 'project' exige.
geraProxy :: [String] -> Exp
geraProxy cols = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (listaTipo cols))
  where
    listaTipo =
      foldr
        (\c acc -> AppT (AppT PromotedConsT (LitT (StrTyLit c))) acc)
        PromotedNilT

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
