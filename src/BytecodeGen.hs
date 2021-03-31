module BytecodeGen where

import SymbolTable
import Syntax

data Inst = Exit Integer

genBytecode :: SymTreeList -> [Inst]
genBytecode trees = concatMap (genGlobalTree . getSymDeclAug) (getSymTreeList trees)

genGlobalTree :: DeclAug SymData -> [Inst]
genGlobalTree (DefFn "main" [] "Int" (LitInt v, _), _) = [Exit v]
genGlobalTree _ = error "haven't implemented this yet"
