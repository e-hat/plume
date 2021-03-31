module VirtualMachine where

import FirstIntermediate
import System.Exit

runInst1Program :: [Inst1] -> IO ()
runInst1Program = mapM_ runInst1

runInst1 :: Inst1 -> IO ()
runInst1 (Exit v) = case v of
                      0 -> exitSuccess
                      _ -> exitWith $ ExitFailure (fromIntegral v)
runInst1 _ = error "haven't implemented this yet"
