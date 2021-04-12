module BytecodeGen where

import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import SymbolTable
import Syntax

data Value
  = Register Integer
  | VBool Bool
  | VInt Integer
  | VFloat Double
  | VByte Char

data Inst = Ret Value | Move Value Value

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

initState :: GState
initState = GState (BytecodeProgram [] M.empty) [1 ..] M.empty M.empty

genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  let statefulProgram =
        traverse (genGlobalTree . getSymDeclAug) (getSymTreeList trees)
      (_, result) = runState statefulProgram initState
   in getCurrentProgram result

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
genGlobalTree (DefFn i [] _ (LitInt v, _), _) = do
  appendLabel i
  appendInst $ Ret (VInt v)
genGlobalTree (DefFn i [] _ (LitBool v, _), _) = do
  appendLabel i
  appendInst $ Ret (VBool v)
genGlobalTree (DefFn i [] _ (LitChar v, _), _) = do
  appendLabel i
  appendInst $ Ret (VByte v)
genGlobalTree (DefFn i [] _ (LitFloat v, _), _) = do
  appendLabel i
  appendInst $ Ret (VFloat v)
genGlobalTree (DefFn fi [] _ (Subs i, _), _) = do
  appendLabel fi
  s <- get
  let rvs = getVarRegisters s
  case M.lookup i rvs of
    Just reg -> appendInst (Ret (Register reg))
    Nothing -> do
      let gvs = getGlobalVars s
      case M.lookup i gvs of
        Nothing -> error $ "ERROR: SYMBOL " ++ i ++ " CANNOT BE FOUND"
        Just v -> appendInst (Ret v)

genDecl :: DeclAug SymData -> State GState ()
genDecl _ = error "haven't implemented this yet"

--genExprInto :: Integer -> ExprAug SymData -> State GState ()
--genExprInto t (BlockExpr ds e, _) = do 
--  traverse_ genDecl ds
--  r <- getNextRegister
--  genExprInto r e
