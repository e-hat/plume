module CodeGen.ARM.Emit where

import CodeGen.ARM.RegAlloc

import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T
import Text.Printf

emit :: Program -> String
emit p = execWriter $ programPrologue >> functionDefinitions
  where
    step :: String -> Func -> Writer String ()
    step "main" f = do
        tell "\n.global main\nmain:\n"
        emitFunction f
    step name f = do
        tell $ "\n" ++ name ++ ":\n"
        emitFunction f
    functionDefinitions :: Writer String ()
    functionDefinitions = mapM_ (uncurry step) (M.assocs (getProgram p))

programPrologue :: Writer String ()
programPrologue = do tell ".text"

emitFunction :: Func -> Writer String ()
emitFunction (Func _ _ insts stackSize) = functionPrologue stackSize >> mapM_ emitInst insts

functionPrologue :: Int -> Writer String ()
functionPrologue stackSize = do
    tell $
        "  // prologue\n"
            ++ "  push {fp, lr}\n"
            ++ "  mov fp, sp\n"
            ++ "  push {r0, r1, r2, r3}\n"
            ++ printf "  // total aligned stack size: %d bytes\n" (nextAlignedByte stackSize)
            ++ printf "  sub sp, sp, #%d\n\n" alignedStackOffset
  where
    alignedStackOffset = nextAlignedByte $ stackSize - 16

functionEpilogue :: Writer String ()
functionEpilogue = do tell $ "  // epilogue\n" ++ "  mov sp, fp\n" ++ "  pop {fp, pc}\n"

emitInst :: Inst -> Writer String ()
emitInst (T.Return Nothing) = functionEpilogue
emitInst (T.Return (Just (T.None t))) = do
    tell
        ( case t of
            (MemoryLoc _ Stack{}) -> "  str r0, "
            _ -> "  mov r0, "
        )
    emitTerm t
    tell "\n"
    functionEpilogue
emitInst _ = do tell ""

emitTerm :: Term -> Writer String ()
emitTerm (MemoryLoc _ (Register n)) = do tell $ "r" ++ show n
emitTerm (MemoryLoc _ (Stack n)) = do tell $ printf "[fp, #-%d]" n
emitTerm (Immediate _ n) = do tell $ "#" ++ show n

nextAlignedByte :: Int -> Int
nextAlignedByte n =
    if n `mod` 8 == 0
        then n
        else (n `div` 8 + 1) * 8
