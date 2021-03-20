module Semantics (validateSemantics) where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

validateSemantics :: Program -> SymTreeList
validateSemantics p =
  let globals = map getASTDeclAug (getProgram p)
      globalScope = buildGlobalScope globals
      buildGlobalSymTree :: SymTable -> DeclAug SpanRec -> DeclAug SymData 
      buildGlobalSymTree tbl l@(Let t i e, sr) = 
        (Let t i (buildSymTreeE mytbl e), SymData mytbl sr)
          where mytbl = Map.delete (getDeclSymbol l) tbl
      buildGlobalSymTree tbl o = buildSymTreeD tbl o
      symTrees = map (buildGlobalSymTree globalScope) globals
   -- comment for debugging, as typecheck is not yet implemented
   --in SymTreeList $ map (SymDeclAug . typecheckD) symTrees
   in SymTreeList $ map SymDeclAug symTrees

buildGlobalScope :: [DeclAug SpanRec] -> SymTable
buildGlobalScope ds =
  let addIfAbsent :: DeclAug SpanRec -> SymTable -> SymTable
      addIfAbsent entry tbl =
        case Map.lookup (getDeclSymbol entry) tbl of
          Just _ -> astSemanticErr entry ("symbol " ++ getDeclSymbol entry ++ " has already been declared in global scope")
          Nothing -> insertDecl entry tbl
   in foldr addIfAbsent Map.empty ds

buildSymTreeD :: SymTable -> DeclAug SpanRec -> DeclAug SymData
buildSymTreeD tbl l@(Let t i e, sr) =
  case Map.lookup (getDeclSymbol l) tbl of
    Just _ -> astSemanticErr l ("overlapping symbol " ++ i)
    Nothing -> (Let t i (buildSymTreeE tbl e), SymData tbl sr)
buildSymTreeD tbl f@(DefFn i ps t e, sr) =
  (DefFn i ps t (buildSymTreeE tbl e), SymData tbl sr)

buildSymTreeE :: SymTable -> ExprAug SpanRec -> ExprAug SymData
-- Literals are easy
-- By definition, they don't have much to do with the Symbol table
buildSymTreeE tbl (LitInt i, sr) = (LitInt i, SymData tbl sr)
buildSymTreeE tbl (LitBool b, sr) = (LitBool b, SymData tbl sr)
buildSymTreeE tbl (LitChar c, sr) = (LitChar c, SymData tbl sr)
buildSymTreeE tbl (LitString s, sr) = (LitString s, SymData tbl sr)
buildSymTreeE tbl (LitFloat f, sr) = (LitFloat f, SymData tbl sr)
buildSymTreeE tbl (Return, sr) = (Return, SymData tbl sr)
-- BlockExpr will be slightly different, needs to accumulate declared symbols
buildSymTreeE tbl (BlockExpr ds e, sr) = 
  (BlockExpr symds syme, SymData tbl sr)
    where
      buildTbls :: [SymTable] -> DeclAug t -> [SymTable]
      buildTbls tbls l@(Let {}, _) = 
        let t = last tbls
        in tbls ++ [insertDecl l t]
      buildTbls tbls _ = tbls ++ [last tbls]
      dtbls = foldl buildTbls [tbl] ds
      symds = zipWith buildSymTreeD dtbls ds
      syme = buildSymTreeE (last dtbls) e

typecheckD :: DeclAug SymData -> DeclAug SymData
typecheckD = undefined

getType :: ExprAug SymData -> Type
getType = undefined
