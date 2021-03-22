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
      SymTreeList $ map (SymDeclAug . typecheckD) symTrees

--SymTreeList $ map SymDeclAug symTrees

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
      checkMain :: SymTable -> SymTable
      checkMain tbl =
        case Map.lookup "main" tbl of
          Just (Single _) -> error "main MUST be a function, not a variable"
          Just m ->
            if m == Many [] "Int"
              then tbl
              else error "wrong type for main function"
          Nothing -> error "missing declaration of main function"
   in checkMain $ foldr addIfAbsent Map.empty ds

buildSymTreeD :: SymTable -> DeclAug SpanRec -> DeclAug SymData
buildSymTreeD tbl l@(Let t i e, sr) =
  case Map.lookup (getDeclSymbol l) tbl of
    Just _ -> astSemanticErr l ("overlapping symbol " ++ i)
    Nothing -> (Let t i (buildSymTreeE tbl e), SymData tbl sr)
buildSymTreeD tbl f@(DefFn i ps t e, sr) =
  let childTbl = foldr insertParam tbl ps
   in (DefFn i ps t (buildSymTreeE childTbl e), SymData tbl sr)
buildSymTreeD tbl r@(Reassign i e, sr) =
  case Map.lookup i tbl of
    Nothing -> astSemanticErr r ("undeclared symbol " ++ i)
    Just _ -> (Reassign i (buildSymTreeE tbl e), SymData tbl sr)

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
buildSymTreeE tbl s@(Subs i, sr) =
  case Map.lookup i tbl of
    Just _ -> (Subs i, SymData tbl sr)
    Nothing -> astSemanticErr s ("undeclared symbol " ++ i)
buildSymTreeE tbl c@(CallExpr i pexprs, sr) =
  case Map.lookup i tbl of
    Nothing -> astSemanticErr c ("undeclared function " ++ i)
    Just (Single _) -> astSemanticErr c ("attempt to call a variable " ++ i ++ " like a function")
    Just _ ->
      (CallExpr i (map (buildSymTreeE tbl) pexprs), SymData tbl sr)

typecheckD :: DeclAug SymData -> DeclAug SymData
typecheckD l@(Let _ _ e, SymData tbl _) =
  let t1 = getDeclType l
      t2 = getType e
   in if t1 == t2
        then l
        else typeError l t1 e t2
typecheckD f@(DefFn i ps rt e, s) =
  let t1 = getDeclType f
      t2 = getType e
   in if t1 == t2
        then (DefFn i ps rt (typecheckE e), s)
        else typeError f t1 e t2
typecheckD r@(Reassign i e, s@(SymData tbl _)) =
  let t1 = lookupSymbolType i tbl
      t2 = getType e
  in if t1 == t2
        then (Reassign i (typecheckE e), s)
        else typeError r t1 e t2

typecheckE :: ExprAug SymData -> ExprAug SymData
typecheckE i@(LitInt _, _) = i
typecheckE s@(LitString _, _) = s
typecheckE f@(LitFloat _, _) = f
typecheckE c@(LitChar _, _) = c
typecheckE b@(LitBool _, _) = b
typecheckE r@(Return, _) = r
typecheckE (BlockExpr ds rexpr, s) =
  (BlockExpr (map typecheckD ds) (typecheckE rexpr), s)
typecheckE s@(Subs _, _) = s
typecheckE c@(innerc@(CallExpr i pexprs), SymData tbl sr) =
  let callEntry = tbl Map.! i
      getParamTypes (Many ts t) = ts
      pTypes = getParamTypes callEntry
      matchParamType :: ExprAug SymData -> Type -> ExprAug SymData
      matchParamType expr t =
        let passedType = getType expr
         in if passedType == t
              then expr
              else typeError c t expr passedType
   in if length pTypes /= length pexprs
        then astSemanticErr (innerc, sr) ("passed " ++ show (length pexprs) ++ " parameter(s) to a function that takes " ++ show (length pTypes) ++ " parameter(s)")
        else (CallExpr i (zipWith matchParamType pexprs pTypes), SymData tbl sr)

getType :: ExprAug SymData -> Type
getType (LitInt _, _) = "Int"
getType (LitString _, _) = "String"
getType (LitFloat _, _) = "Float"
getType (LitChar _, _) = "Char"
getType (LitBool _, _) = "Bool"
getType (Return, _) = "Void"
getType (BlockExpr _ rexpr, _) = getType rexpr
getType (Subs s, SymData tbl _) = lookupSymbolType s tbl
getType (CallExpr i _, SymData tbl _) = lookupSymbolType i tbl
