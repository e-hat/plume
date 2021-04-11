module BytecodeGen where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
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
    getGlobalVars :: S.Set String
  }

setCurrentProgram :: BytecodeProgram -> State GState ()
setCurrentProgram b = modify $ \s -> s { getCurrentProgram = b }

setOpenRegisters :: [Integer] -> State GState () 
setOpenRegisters rs = modify $ \s -> s { getOpenRegisters = rs }

setVarRegisters :: M.Map String Integer -> State GState () 
setVarRegisters vr = modify $ \s -> s { getVarRegisters = vr }

setGlobalVars :: S.Set String -> State GState ()
setGlobalVars gv = modify $ \s -> s { getGlobalVars = gv }

initState :: GState 
initState = GState (BytecodeProgram [] M.empty) [1..] M.empty S.empty

genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees = 
  let statefulProgram = 
        traverse (genGlobalTree . getSymDeclAug) (getSymTreeList trees)
      (_, result) = runState statefulProgram initState
   in getCurrentProgram result

genGlobalTree :: DeclAug SymData -> State GState ()
genGlobalTree _ = error "haven't implemented this yet"
