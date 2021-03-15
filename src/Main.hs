module Main where

import Parser
import Syntax

import qualified Text.Parsec as P
import Text.Show.Pretty
import Options.Applicative

import Data.Semigroup ((<>))

handleNodes :: Either P.ParseError Program -> IO ()
handleNodes (Left err) = error $ show err
handleNodes (Right ns) = dumpIO ns

data CLInput = CLInput 
  { filename :: String 
  , printAST :: Bool }

clinput :: Parser CLInput
clinput = CLInput <$>
  argument str 
    ( metavar "FILE" 
   <> help "Plume source file to be compiled" )
  <*> switch 
    ( long "print-ast"
   <> short 'a'
   <> help "Print the AST generated for this Plume source file" 
   <> showDefault )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (clinput <**> helper)
      ( fullDesc
     <> progDesc "Compile a Plume source file"
     <> header "Plume - strongly, statically typed programming language" )

run :: CLInput -> IO ()
run (CLInput f True) = do
  nodes <- P.parse program f <$> readFile f
  case nodes of 
    Left err -> error $ show err
    Right ast -> dumpIO ast

run (CLInput _ False) = undefined
