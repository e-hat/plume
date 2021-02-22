module Main where

import Parser

import Text.Parsec
  
main :: IO ()
main = do
  putStrLn "Enter some input to parse:"
  result <- parse parseExpr "" <$> getLine
  print result


