module Main where

import Parsing.Parser
import Parsing.Syntax ()
import Semantics.Validation
import Ir.Tac.Translation

import Control.Monad ()
import Data.Semigroup ()
import Options.Applicative
import System.IO
import qualified Text.Parsec as P
import Text.Show.Pretty

data Input
  = ASTInput String
  | ValInput String
  | RunInput String
  | TacInput String
  | CompileInput String

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

tacInput :: Parser Input
tacInput = 
  TacInput 
    <$> strOption
      ( long "tac"
          <> short 't'
          <> metavar "FILENAME"
          <> help "Produce the TAC for a plume program"
      )

compileInput :: Parser Input 
compileInput = 
  CompileInput 
    <$> strOption 
      ( long "compile"
          <> short 'c'
          <> metavar "FILENAME"
          <> help "Compile a plume program"
      )

compileOptions :: Parser Input
compileOptions = astInput <|> valInput <|> tacInput <|> compileInput

runArg :: Parser Input
runArg =
  RunInput <$> argument str (metavar "FILE")

input :: Parser Input
input =
  hsubparser
    ( command
        "compile"
        ( info
            (compileOptions <**> helper)
            (progDesc "Compile a Plume source file")
        )
        <> command
          "run"
          ( info
              (runArg <**> helper)
              (progDesc "Run a Plume program")
          )
    )

main :: IO ()
main = run =<< execParser opts
 where
  opts =
    info
      (input <**> helper)
      (progDesc "Plume - strongly, statically typed programming language")

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
run RunInput{} = do
  hPutStrLn stderr "The Plume VM has been deprecated and is no longer available for use."
run (TacInput f) = do 
  nodes <- P.parse program f <$> readFile f
  case nodes of 
    Left err -> print err 
    Right p -> case validateSemantics p of 
      Left err -> putStrLn err
      Right trees -> print $ translate trees
run _ = error "unimplemented"
