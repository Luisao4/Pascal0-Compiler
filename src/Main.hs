{-

Main should read all the input and read everything in lower case (case insensitive)




-}



module Main where

import Lexer
import Parser
import Data.Char
import AST
--import Typecheck
--import PMips

main :: IO ()
main = do
  txt <- getContents
  mapM_ print(intermediate( parse $ alexScanTokens ( caseSensitve txt False)))


{- CODIGO PARA INTRODUZIR O TYPECHECK
  let a = (parse $ alexScanTokens ( caseSensitve txt False))
  --depois chama o mips
  if (checkProg a == True) then
    mapM_ print(intermediate a)  -- PMips disto
    print("Verificado")
  else
    error "invalid type"
-}

caseSensitve :: String -> Bool -> String
caseSensitve [] b = []
caseSensitve (x:xs) True
  | x == '\'' = x : ( caseSensitve xs False )
  | otherwise = x : ( caseSensitve xs True )
caseSensitve (x:xs) False
  | x == '\'' = x : ( caseSensitve xs True )
  | otherwise = (toLower x) : ( caseSensitve xs False )