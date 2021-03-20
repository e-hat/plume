module Semantics where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

buildGlobalScopes :: Program -> [(Decl SpanRec, SymData)]
buildGlobalScopes (Program astdaugs) = buildGlobalScopesImpl (map getASTDeclAug astdaugs)
  where
    buildGlobalScopesImpl :: [DeclAug SpanRec] -> [(Decl SpanRec, SymData)]
    buildGlobalScopesImpl ds = 
      let addIfAbsent :: DeclAug SpanRec -> SymTable -> SymTable 
          addIfAbsent entry tbl = 
            case Map.lookup (getDeclSymbol entry) tbl of
              Just _ -> astSemanticErr entry ("symbol " ++ getDeclSymbol entry ++ " has already been declared in global scope")
              Nothing -> insertDecl entry tbl
          symMap = foldr addIfAbsent Map.empty ds
          buildEntry :: DeclAug SpanRec -> (Decl SpanRec, SymData)
          buildEntry (d, sr) = (d, SymData symMap sr)
      in map buildEntry ds 
