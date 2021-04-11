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
  | VFloat Float
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

initState :: GState
initState = GState (BytecodeProgram [] M.empty) [1 ..] M.empty M.empty

genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  let statefulProgram =
        traverse (genGlobalTree . getSymDeclAug) (getSymTreeList trees)
      (_, result) = runState statefulProgram initState
   in getCurrentProgram result

genGlobalTree :: DeclAug SymData -> State GState ()
genGlobalTree (Let "Int" i (LitInt v, _), _) = do
  s <- get
  let vars = getGlobalVars s
  setGlobalVars $ M.insert i (VInt v) vars
genGlobalTree (DefFn i [] "Int" (LitInt v, _), _) = do
  appendLabel i
  appendInst $ Ret (VInt v)
genGlobalTree (DefFn fi [] "Int" (Subs i, _), _) = do
  appendLabel fi
  s <- get
  let rvs = getVarRegisters s
  case M.lookup i rvs of
    Just _ -> error "haven't implemented this yet"
    Nothing -> do
      let gvs = getGlobalVars s
      case M.lookup i gvs of
        Nothing -> error $ "ERROR: SYMBOL " ++ i ++ " CANNOT BE FOUND"
        Just v -> appendInst (Ret v)
