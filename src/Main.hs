module Main where

import Parser

import Text.Parsec as P
  
main :: IO ()
main = do
  putStrLn "Enter some input to parse:"
  result <- P.parse parseExpr "" <$> getLine
  print result


