module Main where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import Parser
import Semantics
import Syntax
import qualified Text.Parsec as P
import Text.Show.Pretty

data CLInput = CLInput
  { filename :: String,
    printAST :: Bool
  }

data Input = ASTInput String | ValInput String

astInput :: Parser Input
astInput =
  ASTInput
    <$> strOption
      ( long "print-ast"
          <> short 'a'
          <> metavar "FILENAME"
          <> help "Print the AST of the input file"
      )

valInput :: Parser Input
valInput =
  ValInput
    <$> strOption
      ( long "validate-sem"
          <> short 'v'
          <> metavar "FILENAME"
          <> help "Validate the semantics of the input file"
      )

input :: Parser Input 
input = astInput <|> valInput

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (input <**> helper)
        ( fullDesc
            <> progDesc "Compile a Plume source file"
            <> header "Plume - strongly, statically typed programming language"
        )

run :: Input -> IO ()
run (ASTInput f) = do
  nodes <- P.parse program f <$> readFile f
  case nodes of
    Left err -> print err
    Right p -> dumpIO p
run (ValInput f) = do
  nodes <- P.parse program f <$> readFile f
  case nodes of
    Left err -> print err
    Right p -> case validateSemantics p of 
             Left err -> putStrLn err
             Right _ -> putStrLn ("Validation of " ++ f ++ " successful.")
