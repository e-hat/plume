module Semantics (validateSemantics) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

validateSemantics :: Program -> [SymDeclAug]
validateSemantics p =
  let globals = map getASTDeclAug (getProgram p)
      globalScope = buildGlobalScope globals
      symTrees = map (buildSymTreeD globalScope) globals
   in map (SymDeclAug . typecheckD) symTrees

buildGlobalScope :: [DeclAug SpanRec] -> SymTable
buildGlobalScope ds =
  let addIfAbsent :: DeclAug SpanRec -> SymTable -> SymTable
      addIfAbsent entry tbl =
        case Map.lookup (getDeclSymbol entry) tbl of
          Just _ -> astSemanticErr entry ("symbol " ++ getDeclSymbol entry ++ " has already been declared in global scope")
          Nothing -> insertDecl entry tbl
   in foldr addIfAbsent Map.empty ds

buildSymTreeD :: SymTable -> DeclAug SpanRec -> DeclAug SymData
buildSymTreeD = undefined

buildSymTreeE :: SymTable -> ExprAug SpanRec -> ExprAug SymData
buildSymTreeE = undefined

typecheckD :: DeclAug SymData -> DeclAug SymData
typecheckD = undefined

getType :: ExprAug SymData -> Type
getType = undefined
