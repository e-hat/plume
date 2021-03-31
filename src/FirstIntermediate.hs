module FirstIntermediate where

import SymbolTable
import Syntax

data Inst1 = Exit Integer

genInst1 :: SymTreeList -> [Inst1]
genInst1 trees = concatMap (genGlobalTree . getSymDeclAug) (getSymTreeList trees)

genGlobalTree :: DeclAug SymData -> [Inst1]
genGlobalTree (DefFn "main" [] "Int" (LitInt v, _), _) = [Exit v]
genGlobalTree _ = error "haven't implemented this yet"
