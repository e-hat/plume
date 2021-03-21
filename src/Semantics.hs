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
        where
          mytbl = Map.delete (getDeclSymbol l) tbl
      buildGlobalSymTree tbl o = buildSymTreeD tbl o
      checkGlobalLet :: DeclAug SpanRec -> DeclAug SpanRec
      checkGlobalLet l@(Let _ _ e, sr) =
        if isLit e
          then l
          else astSemanticErr l "a global let MUST be a literal value"
      checkGlobalLet d = d
      symTrees = map (buildGlobalSymTree globalScope . checkGlobalLet) globals
   in -- comment for debugging, as typecheck is not yet implemented
      --SymTreeList $ map (SymDeclAug . typecheckD) symTrees
      SymTreeList $ map SymDeclAug symTrees

isLit :: ExprAug SpanRec -> Bool
isLit (LitInt _, _) = True
isLit (LitFloat _, _) = True
isLit (LitString _, _) = True
isLit (LitChar _, _) = True
isLit (LitBool _, _) = True
isLit _ = False

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
  let childTbl = foldr insertParam tbl ps
   in (DefFn i ps t (buildSymTreeE childTbl e), SymData tbl sr)

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
typecheckD l@(Let _ _ e, SymData sr tbl) =
  let t1 = getEntryType $ getDeclEntry l
      t2 = getType e
   in if t1 == t2
        then l
        else typeError l t1 e t2

getType :: ExprAug SymData -> Type
getType (LitInt _, _) = "Int"
getType (LitString _, _) = "String"
getType (LitFloat _, _) = "Float"
getType (LitChar _, _) = "Char"
getType (LitBool _, _) = "Bool"
getType (Return, _) = "Void"
