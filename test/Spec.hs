module Main (main) where

import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import System.Exit (exitFailure)
import TypedQL.Schema

type Vendors =
  '[ "vendor_code" ':= 'TText
   , "open_rate" ':= 'TDouble
   ]

-- Testes de tipo: se estas linhas compilam, a propriedade vale.
tipoDaColuna :: Proxy (TypeOf "open_rate" Vendors) -> Proxy 'TDouble
tipoDaColuna = id

nomes :: Proxy (Names Vendors) -> Proxy '["vendor_code", "open_rate"]
nomes = id

projecaoPreservaTipo ::
  Proxy (Project '["open_rate"] Vendors) -> Proxy '["open_rate" ':= 'TDouble]
projecaoPreservaTipo = id

renomeia ::
  Proxy (Rename "open_rate" "taxa" Vendors) ->
  Proxy '["vendor_code" ':= 'TText, "taxa" ':= 'TDouble]
renomeia = id

-- Testes de valor.
casos :: [(String, Bool)]
casos =
  [ ("demote STInt", demote STInt == TInt)
  , ("parse text", fmap show (parseSqlType "TEXT") == Just "TText")
  , ("parse invalido", maybe True (const False) (parseSqlType "blob"))
  ]

main :: IO ()
main = do
  let ruins = [n | (n, ok) <- casos, not ok]
  mapM_ (\n -> putStrLn ("FALHOU: " ++ n)) ruins
  unless (null ruins) exitFailure
  putStrLn ("OK: " ++ show (length casos) ++ " testes de valor + 4 testes de tipo")
