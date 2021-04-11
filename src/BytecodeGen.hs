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

registerStore = [1 ..]

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

genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  foldr
    (genGlobalTree . getSymDeclAug)
    (BytecodeProgram [] M.empty)
    (getSymTreeList trees)

genGlobalTree :: DeclAug SymData -> BytecodeProgram -> BytecodeProgram
genGlobalTree
  (DefFn "main" [] "Int" (LitInt v, _), _)
  (BytecodeProgram is lt) = BytecodeProgram (is ++ [Ret $ VInt v]) lt
genGlobalTree _ _ = error "haven't implemented this yet"
