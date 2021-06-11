module Main where

import Bytecode.Generation
import Bytecode.Types()
import Parsing.Parser
import Parsing.Syntax()
import RegAlloc.LiveIntervals
import Semantics.Validation
import VirtualMachine.VirtualMachine

import Control.Monad()
import Data.Semigroup()
import Options.Applicative
import System.IO
import qualified Text.Parsec as P
import Text.Show.Pretty

data Input
    = ASTInput String
    | ValInput String
    | RunInput String
    | PrintBytecodeInput String
    | LiveIntervalInput String

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

printBytecodeInput :: Parser Input
printBytecodeInput =
    PrintBytecodeInput
        <$> strOption
            ( long "print-bytecode"
                <> short 'b'
                <> metavar "FILENAME"
                <> help "Print the bytecode generated for the input file"
            )

liveIntervalInput :: Parser Input
liveIntervalInput =
    LiveIntervalInput
        <$> strOption
            ( long "live-intervals"
                <> short 'l'
                <> metavar "FILENAME"
                <> help "Display the virtual registers' live intervals for the bytecode generated from the input file"
            )

compileOptions :: Parser Input
compileOptions = astInput <|> valInput <|> printBytecodeInput <|> liveIntervalInput

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
run (PrintBytecodeInput f) = do
    nodes <- P.parse program f <$> readFile f
    case nodes of
        Left err -> print err
        Right p -> case validateSemantics p of
            Left err -> putStrLn err
            Right trees -> print $ genBytecode trees
run (RunInput f) = do
    hPutStrLn stderr "Warning: running Plume bytecode with the Plume VM is deprecated"
    nodes <- P.parse program f <$> readFile f
    case nodes of
        Left err -> print err
        Right p -> case validateSemantics p of
            Left err -> putStrLn err
            Right trees -> runBytecode $ genBytecode trees
run (LiveIntervalInput f) = do
    nodes <- P.parse program f <$> readFile f
    case nodes of
        Left err -> print err
        Right p -> case validateSemantics p of
            Left err -> putStrLn err
            Right trees -> print $ liveIntervals $ genBytecode trees
