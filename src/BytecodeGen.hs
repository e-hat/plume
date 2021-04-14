module BytecodeGen
  ( Inst (..),
    Value (..),
    BytecodeProgram (..),
    genBytecode,
    retReg,
  )
where

import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import SymbolTable
import Syntax

-- types of values that can be moved around
data Value
  = Register Integer
  | VBool Bool
  | VInt Integer
  | VFloat Double
  | VByte Char

data Inst
  = Ret
  | Move Value Value
  | Add Value Value Value
  | Sub Value Value Value
  | Mul Value Value Value
  | Div Value Value Value
  | Neg Value Value

data BytecodeProgram = BytecodeProgram
  { getInstructions :: [Inst],
    getLabelTable :: M.Map String Integer
  }

data GState = GState
  { getCurrentProgram :: BytecodeProgram,
    getOpenRegisters :: [Integer],
    getVarRegisters :: M.Map String Integer,
    getGlobalVars :: M.Map String Value
  }

-- helper functions for managing state
setCurrentProgram :: BytecodeProgram -> State GState ()
setCurrentProgram b = modify $ \s -> s {getCurrentProgram = b}

appendInst :: Inst -> State GState ()
appendInst i = do
  s <- get
  let prog = getCurrentProgram s
  setCurrentProgram $ prog {getInstructions = getInstructions prog ++ [i]}

appendLabel :: String -> State GState ()
appendLabel l = do
  s <- get
  let prog = getCurrentProgram s
  let tbl = getLabelTable prog
  let loc = toInteger (1 + length (getInstructions prog))
  setCurrentProgram $ prog {getLabelTable = M.insert l loc tbl}

setOpenRegisters :: [Integer] -> State GState ()
setOpenRegisters rs = modify $ \s -> s {getOpenRegisters = rs}

setVarRegisters :: M.Map String Integer -> State GState ()
setVarRegisters vr = modify $ \s -> s {getVarRegisters = vr}

setGlobalVars :: M.Map String Value -> State GState ()
setGlobalVars gv = modify $ \s -> s {getGlobalVars = gv}

getNextRegister :: State GState Integer
getNextRegister = do
  s <- get
  let rs = getOpenRegisters s
  let result = head rs
  setOpenRegisters $ tail rs
  return result

retReg :: Integer
retReg = 0

retVal :: Value -> Inst
retVal v = Move v (Register retReg)

initState :: GState
initState = GState (BytecodeProgram [] M.empty) [retReg + 1 ..] M.empty M.empty

-------------------------------------------------------------------------------
----------------------------- BYTECODE GENERATION -----------------------------
-------------------------------------------------------------------------------
genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  let statefulProgram =
        traverse (genGlobalTree . getSymDeclAug) (getSymTreeList trees)
      (_, result) = runState statefulProgram initState
   in getCurrentProgram result

-- this will need to be rewritten using memory constructs instead
addGlobalVar :: String -> Value -> State GState ()
addGlobalVar i v = do
  s <- get
  let vars = getGlobalVars s
  setGlobalVars $ M.insert i v vars

genGlobalTree :: DeclAug SymData -> State GState ()
genGlobalTree (Let _ i (LitInt v, _), _) = addGlobalVar i (VInt v)
genGlobalTree (Let _ i (LitBool v, _), _) = addGlobalVar i (VBool v)
genGlobalTree (Let _ i (LitChar v, _), _) = addGlobalVar i (VByte v)
genGlobalTree (Let _ i (LitFloat v, _), _) = addGlobalVar i (VFloat v)
genGlobalTree (DefFn i [] _ e, _) = do
  appendLabel i
  moveExprInto retReg e
  appendInst Ret

genDecl :: DeclAug SymData -> State GState ()
genDecl (Let _ i e, _) = do
  r <- getNextRegister
  moveExprInto r e
  s <- get
  let vrs = getVarRegisters s
  setVarRegisters (M.insert i r vrs)
genDecl (Reassign i e, _) = do
  s <- get
  let rvs = getVarRegisters s
  case M.lookup i rvs of
    Just r -> moveExprInto r e
    Nothing -> error "I need to implement memory to make reassignments of global variables work!"
genDecl _ = error "haven't implemented this yet"

-- puts the result of an expression into the specified register
moveExprInto :: Integer -> ExprAug SymData -> State GState ()
moveExprInto t (BlockExpr ds e, _) = do
  traverse_ genDecl ds
  moveExprInto t e
moveExprInto t b@(BinOp _ l r, _) = do
  lval <- genExprValue l
  rval <- genExprValue r
  appendInst (binOpMapping b lval rval (Register t))
moveExprInto t e = do
  v <- genExprValue e
  appendInst (Move v (Register t))

genExprValue :: ExprAug SymData -> State GState Value
genExprValue (LitInt v, _) = return (VInt v)
genExprValue (LitBool v, _) = return (VBool v)
genExprValue (LitChar v, _) = return (VByte v)
genExprValue (LitFloat v, _) = return (VFloat v)
genExprValue (Subs i, _) = do
  s <- get
  let rvs = getVarRegisters s
  case M.lookup i rvs of
    Just reg -> return (Register reg)
    Nothing -> do
      let gvs = getGlobalVars s
      case M.lookup i gvs of
        Nothing -> error $ "ERROR: SYMBOL " ++ i ++ " CANNOT BE FOUND"
        Just v -> return v
genExprValue b@(BinOp _ l r, _) = do
  n <- getNextRegister
  moveExprInto n b
  return (Register n)

binOpMapping :: ExprAug SymData -> (Value -> Value -> Value -> Inst)
binOpMapping (BinOp Plus _ _, _) = Add
binOpMapping (BinOp Minus _ _, _) = Sub
binOpMapping (BinOp Multiply _ _, _) = Mul
binOpMapping (BinOp Divide _ _, _) = Div
