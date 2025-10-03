module Main where

import qualified CodeGen.ARM.Emit as ARMEmit
import CodeGen.ARM.RegAlloc
import CodeGen.Wasm.Emit
import CodeGen.Wasm.Translation
import Control.Monad ()
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ()
import Ir.Tac.Translation
import Options.Applicative
import Parsing.Parser
import Parsing.Syntax (Op(Negate))
import Semantics.Validation
import System.IO
import qualified Text.Parsec as P
import Text.Show.Pretty
import Ir.Cfg.Types
import qualified Ir.Tac.Types as T
import qualified Ir.Tac.Translation as Tr

data Input
    = ASTInput String
    | ValInput String
    | RunInput String
    | TacInput String
    | CompileInputWasm String
    | CompileInputARM String
    | Whatever String

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

compileInputWasm :: Parser Input
compileInputWasm =
    CompileInputWasm
        <$> strOption
            ( long "wasm"
                <> short 'w'
                <> metavar "FILENAME"
                <> help "Compile a plume program to WebAssembly"
            )

compileInputARM :: Parser Input
compileInputARM =
    CompileInputARM
        <$> strOption
            ( long "arm"
                <> metavar "FILENAME"
                <> help "Compile a plume program to ARM assembly"
            )

compileOptions :: Parser Input
compileOptions = astInput <|> valInput <|> tacInput <|> compileInputWasm <|> compileInputARM

runArg :: Parser Input
runArg =
    RunInput <$> argument str (metavar "FILE")

whateverArg :: Parser Input 
whateverArg = Whatever <$> argument str (metavar "FILE")
    

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
            -- For testing random functionality thats in development
            <> command "whatever"
                ( info (whateverArg <**> helper) (progDesc "Do whatever!"))
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
            Right trees -> print $ toTac trees
run (CompileInputWasm f) = do
    nodes <- P.parse program f <$> readFile f
    case nodes of
        Left err -> hPrint stderr err
        Right p -> case validateSemantics p of
            Left err -> hPutStrLn stderr err
            Right trees ->
                let tac = toTac trees
                    output = runPut $ emit $ toWasm tac
                 in BL.putStr output
run (CompileInputARM f) = do
    nodes <- P.parse program f <$> readFile f
    case nodes of
        Left err -> hPrint stderr err
        Right p -> case validateSemantics p of
            Left err -> hPutStrLn stderr err
            Right trees ->
                let tac = toTac trees
                    postRegAlloc = naiveRegAlloc tac
                 in putStrLn $ ARMEmit.emit postRegAlloc
run Whatever{} = 
    let bbs = [BasicBlock "start" [T.Assignment (T.None (T.Subs (T.Local 0 "String"))) (T.Un Negate (T.LitInt 5))] (Jump "end"), BasicBlock "end" [] Return]
     in print $ fromList bbs
