{-

Main should read all the input and read everything in lower case (case insensitive)




-}



module Main where

import Lexer
import Parser
import Data.Char


main :: IO ()
main = do
  txt <- getContents
  print (parse $ alexScanTokens (caseSensitve txt False))

caseSensitve :: String -> Bool -> String
caseSensitve [] b = []
caseSensitve (x:xs) True
  | x == '\'' = x : ( caseSensitve xs False )
  | otherwise = x : ( caseSensitve xs True )
caseSensitve (x:xs) False
  | x == '\'' = x : ( caseSensitve xs True )
  | otherwise = (toLower x) : ( caseSensitve xs False )
