module Main (main) where

import Data.Proxy (Proxy (..))
import TypedQL.Schema

-- | Esquema de exemplo, declarado no nivel de tipos.
type Vendors =
  '[ "vendor_code" ':= 'TText
   , "vendor_name" ':= 'TText
   , "open_rate" ':= 'TDouble
   , "defeitos" ':= 'TInt
   ]

-- Consulta valida no nivel de tipos: o compilador aceita.
projecaoOk :: Proxy (Project '["vendor_code", "open_rate"] Vendors)
projecaoOk = Proxy

-- Descomente para ver o erro de compilacao com a mensagem customizada:
-- projecaoRuim :: Proxy (Project '["vendor_cod"] Vendors)
-- projecaoRuim = Proxy

main :: IO ()
main = do
  putStrLn "TypedQL 0.1 - modulo 1 (Schema)"
  putStrLn "Reflexao (tipo -> valor):"
  print (demote (singSqlType @'TDouble))
  putStrLn "Reificacao (valor -> tipo, escondido em existencial):"
  print (parseSqlType "text")
  print (parseSqlType "blob")
  putStrLn "Projecao validada em tempo de compilacao: ok"
  print projecaoOk `seq` pure ()
