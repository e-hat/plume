module VirtualMachine where

import BytecodeGen
import System.Exit

runBytecode :: BytecodeProgram -> IO ()
runBytecode b@(BytecodeProgram is tbl) = mapM_ (runInst b) is

runInst :: BytecodeProgram -> Inst -> IO ()
runInst _ (Ret v) = case v of
                      0 -> exitSuccess
                      _ -> exitWith $ ExitFailure (fromIntegral v)
runInst _ _ = error "haven't implemented this yet"
