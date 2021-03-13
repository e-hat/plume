module Main where

import Parser
import Syntax

import Text.Parsec as P

handleNodes :: Either ParseError Program -> IO ()
handleNodes (Left err) = error $ show err
handleNodes (Right ns) = print ns
  
main :: IO ()
main = do
  fname <- getLine
  nodes <- P.parse program fname <$> readFile fname
  handleNodes nodes

