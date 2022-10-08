module CodeGen.ARM.Emit where

import CodeGen.ARM.RegAlloc
import Data.List
import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T
import Text.Printf

emit :: Program -> String
emit p = programPrologue ++ functionDefinitions
  where
    step :: String -> String -> Func -> String
    step accum "main" f = accum ++ "\n.global main\nmain:\n" ++ emitFunction f
    step accum name f = accum ++ "\n" ++ name ++ ":\n" ++ emitFunction f
    functionDefinitions = M.foldlWithKey step "" $ getProgram p

programPrologue :: String
programPrologue = ".text"

emitFunction :: Func -> String
emitFunction (Func _ _ insts stackSize) = functionPrologue stackSize ++ intercalate "\n" (map emitInst insts)

functionPrologue :: Int -> String
functionPrologue stackSize =
    "  push {fp, lr}\n"
        ++ "  mov fp, sp\n"
        ++ "  push {r0, r1, r2, r3}\n"
        ++ printf "  sub sp, sp, #%d\n" alignedStackOffset
  where
    alignedStackOffset = nextAlignedByte 8 $ stackSize - 16

functionEpilogue :: String
functionEpilogue = "  mov sp, fp\n" ++ "  pop {fp, pc}"

emitInst :: Inst -> String
emitInst (T.Return Nothing) = functionEpilogue
emitInst _ = ""

nextAlignedByte :: Int -> Int -> Int
nextAlignedByte alignment n =
    if n `mod` alignment == 0
        then n
        else (n `div` alignment + 1) * alignment
