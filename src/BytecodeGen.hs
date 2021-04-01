module BytecodeGen where

import qualified Data.Set as S
import SymbolTable
import Syntax

data Inst = Ret Integer

data BytecodeProgram = BytecodeProgram
  { getInstructions :: [Inst],
    getLabelTable :: S.Set (String, Integer)
  }

genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  foldr
    (genGlobalTree . getSymDeclAug)
    (BytecodeProgram [] S.empty)
    (getSymTreeList trees)

genGlobalTree :: DeclAug SymData -> BytecodeProgram -> BytecodeProgram
genGlobalTree
  (DefFn "main" [] "Int" (LitInt v, _), _)
  (BytecodeProgram is lt) = BytecodeProgram (is ++ [Ret v]) lt
genGlobalTree _ _ = error "haven't implemented this yet"
