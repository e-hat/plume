module Main where

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import Parser
import Semantics
import Syntax
import VirtualMachine
import BytecodeGen
import qualified Text.Parsec as P
import Text.Show.Pretty

data Input = ASTInput String | ValInput String | RunInst1Input String

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

inst1Input :: Parser Input 
inst1Input = 
  RunInst1Input 
  <$> strOption
    ( long "run-inst1-bytecode"
        <> metavar "FILENAME"
        <> help "Run the Inst1 bytecode using the VM for the given input file"
    )

input :: Parser Input 
input = astInput <|> valInput <|> inst1Input

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
run (RunInst1Input f) = do 
  nodes <- P.parse program f <$> readFile f 
  case nodes of 
    Left err -> print err 
    Right p -> case validateSemantics p of 
                 Left err -> putStrLn err 
                 Right trees -> runBytecode $ genBytecode trees
