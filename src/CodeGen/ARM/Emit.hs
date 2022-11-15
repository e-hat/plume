module CodeGen.ARM.Emit where

import CodeGen.ARM.RegAlloc

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T
import qualified Parsing.Syntax as S
import Text.Printf

emit :: Program -> String
emit p = execWriter $ evalStateT (lift programPrologue >> functionDefinitions) 0
  where
    step :: String -> Func -> StateT Int (Writer String) ()
    step "main" f = do
        tell "\n.global main\nmain:\n"
        emitFunction f
    step name f = do
        tell $ "\n" ++ name ++ ":\n"
        emitFunction f
    functionDefinitions :: StateT Int (Writer String) ()
    functionDefinitions = mapM_ (uncurry step) (M.assocs (getProgram p))

programPrologue :: Writer String ()
programPrologue = do tell ".text"

emitFunction :: Func -> StateT Int (Writer String) ()
emitFunction (Func _ _ insts stackSize) = lift (functionPrologue stackSize) >> mapM_ emitInst insts

functionPrologue :: Int -> Writer String ()
functionPrologue stackSize = do
    tell $
        "  // prologue\n"
            ++ "  push {fp, lr}\n"
            ++ "  mov fp, sp\n"
            ++ "  push {r1, r2, r3, r4}\n"
            ++ printf "  // total aligned stack size: %d bytes\n" (nextAlignedByte stackSize)
            ++ printf "  sub sp, sp, #%d\n" alignedStackOffset
            ++ "  // end of prologue\n\n"
  where
    alignedStackOffset = nextAlignedByte $ stackSize - 16

functionEpilogue :: Writer String ()
functionEpilogue = do tell $ "\n  // epilogue\n" ++ "  pop {r1, r2, r3, r4}\n  mov sp, fp\n  pop {fp, pc}\n"

getNextLabel :: StateT Int (Writer String) String
getNextLabel = do
    current <- get
    put (current + 1)
    return (printf ".L%d" current)

emitInst :: Inst -> StateT Int (Writer String) ()
emitInst (T.Return Nothing) = lift functionEpilogue
emitInst (T.Return (Just mathOp)) = do
    lift $ computeIntoRegister 0 mathOp
    lift functionEpilogue
emitInst (T.Assignment (T.None (MemoryLoc _ (Stack loc))) rhs) = do
    lift $ computeIntoRegister 7 rhs
    lift $ writeRegisterToMem 7 loc
emitInst (T.IgnoreReturnValCall (T.FuncCall (name, args))) = do
    lift $ zipWithM_ writeTermToRegister [1 ..] args
    tell $ printf "  bl %s\n" name
emitInst (T.Block insts) = mapM_ emitInst insts
emitInst (T.Cond cond cons alt) = do
    altLabel <- getNextLabel
    exitLabel <- getNextLabel
    lift $ computeIntoRegister 7 cond
    tell "  cmp r7, #0\n"
    tell $ printf "  beq %s\n\n" altLabel
    mapM_ emitInst cons
    tell $ printf "  b %s\n\n" exitLabel
    tell $ printf "%s:\n" altLabel
    mapM_ emitInst alt
    tell $ printf "%s:\n" exitLabel
emitInst (T.While cond body) = do
    topLabel <- getNextLabel
    exitLabel <- getNextLabel
    tell $ printf "%s:\n" topLabel
    lift $ computeIntoRegister 7 cond
    tell "  cmp r7, #0\n"
    tell $ printf "  beq %s\n" exitLabel
    mapM_ emitInst body
    tell $ printf "  b %s\n" topLabel
    tell $ printf "%s:\n" exitLabel
emitInst _ = do tell ""

writeTermToRegister :: Int -> Term -> Writer String ()
writeTermToRegister register t = do
    tell
        ( case t of
            (MemoryLoc _ Stack{}) -> printf "  ldr r%d, " register
            _ -> printf "  mov r%d, " register
        )
    emitTerm t
    tell "\n"

stackLoc :: Int -> String
stackLoc = printf "[fp, #-%d]"

writeRegisterToMem :: Int -> Int -> Writer String ()
writeRegisterToMem reg loc = do
    tell $ printf "  str r%d, %s\n" reg (stackLoc loc)

-- registers r1 and r2 are going to be our intermediates for operations
computeIntoRegister :: Int -> T.Expr Term -> Writer String ()
computeIntoRegister dst (T.None t) = writeTermToRegister dst t
computeIntoRegister dst (T.Un op t) = do
    writeTermToRegister 5 t
    tell (printf "  %s r%d, r%d\n" (opToArm op) dst (5 :: Int))
computeIntoRegister dst (T.Bin l op r)
    | isComparison op = do
        writeTermToRegister 5 l
        writeTermToRegister 6 r
        tell "  cmp r5, r6\n"
        tell $ printf "  mov r%d, #0\n" dst
        tell $ printf "  %s r%d, #1\n" (opToArm op) dst
    | otherwise = do
        writeTermToRegister 5 l
        writeTermToRegister 6 r
        tell (printf "  %s r%d, r%d, r%d\n" (opToArm op) dst (5 :: Int) (6 :: Int))
computeIntoRegister dst (T.FuncCallExpr _ (T.FuncCall (name, args))) =
    if length args > 4
        then error "plume doesn't support function calls with more than 3 arguments"
        else do
            zipWithM_ writeTermToRegister [1 ..] args
            tell $ printf "  bl %s\n" name
            when (dst /= 0) $ tell $ printf "  mov r%d, r%d\n" dst (0 :: Int)

isComparison :: S.Op -> Bool
isComparison S.Less = True
isComparison S.Leq = True
isComparison S.Greater = True
isComparison S.Geq = True
isComparison S.NotEqual = True
isComparison S.Equal = True
isComparison _ = False

opToArm :: S.Op -> String
opToArm S.Negate = "neg"
opToArm S.Not = "mvn"
opToArm S.Plus = "add"
opToArm S.Minus = "sub"
opToArm S.Multiply = "mul"
opToArm S.Divide = "sdiv"
opToArm S.Or = "orr"
opToArm S.And = "and"
opToArm S.Less = "movlt"
opToArm S.Leq = "movle"
opToArm S.Greater = "movgt"
opToArm S.Geq = "movge"
opToArm S.Equal = "moveq"
opToArm S.NotEqual = "movne"

emitTerm :: Term -> Writer String ()
emitTerm (MemoryLoc _ (Register n)) = do tell $ "r" ++ show n
emitTerm (MemoryLoc _ (Stack n)) = do tell $ printf "[fp, #-%d]" n
emitTerm (Immediate _ n) = do tell $ "#" ++ show n

nextAlignedByte :: Int -> Int
nextAlignedByte n =
    if n `mod` 8 == 0
        then n
        else (n `div` 8 + 1) * 8
