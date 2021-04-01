module BytecodeGen where

import qualified Data.Map.Strict as M
import SymbolTable
import Syntax

data Inst = Ret Integer

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
  (BytecodeProgram is lt) = BytecodeProgram (is ++ [Ret v]) lt
genGlobalTree _ _ = error "haven't implemented this yet"
