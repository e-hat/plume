module VirtualMachine where

import BytecodeGen
import System.Exit

runBytecode :: [Inst] -> IO ()
runBytecode = mapM_ runInst

runInst :: Inst -> IO ()
runInst (Ret v) = case v of
                      0 -> exitSuccess
                      _ -> exitWith $ ExitFailure (fromIntegral v)
runInst _ = error "haven't implemented this yet"
