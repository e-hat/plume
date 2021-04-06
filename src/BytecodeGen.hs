module BytecodeGen where

import qualified Data.Map.Strict as M
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
