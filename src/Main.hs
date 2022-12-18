{-
Convert a String to its lowercase version while preserving the casing inside
True -> Preserve , False -> Put into lowercase
-}

module Main where

import Lexer
import Parser
import Data.Char
import AST
--import Typecheck
--import PMips

-- CODIGO PARA OBTER O PRINT DO INTERMEDIATE CODE
main :: IO ()
main = do
  txt <- getContents
  mapM_ print(intermediate( parse $ alexScanTokens ( caseSensitve txt False)))

caseSensitve :: String -> Bool -> String
caseSensitve [] b = []
caseSensitve (x:xs) True
  | x == '\'' = x : ( caseSensitve xs False )
  | otherwise = x : ( caseSensitve xs True )
caseSensitve (x:xs) False
  | x == '\'' = x : ( caseSensitve xs True )
  | otherwise = (toLower x) : ( caseSensitve xs False )

  {- CODIGO PARA OBTER APENAS O PRINT DO PARSER
main :: IO ()
main = do
  txt <- getContents
  print (parse $ alexScanTokens ( caseSensitve txt False))
-}

{- CODIGO PARA INTRODUZIR O TYPECHECK
  let a = (parse $ alexScanTokens ( caseSensitve txt False))
  --depois chama o mips
  if (checkProg a == True) then
    mapM_ print(intermediate a)  -- PMips disto
    print("Verificado")
  else
    error "invalid type"
-}
