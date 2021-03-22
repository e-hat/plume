module Semantics (validateSemantics) where

import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

-- this function performs scoping (symbol table building + catching scoping errors)
-- & typechecking for a given AST
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

-- this function is for verifiying that all Lets in global scope
-- are literal expressions
isLit :: ExprAug SpanRec -> Bool
isLit (LitInt _, _) = True
isLit (LitFloat _, _) = True
isLit (LitString _, _) = True
isLit (LitChar _, _) = True
isLit (LitBool _, _) = True
isLit _ = False

--------------------------------------------------------------------------------
----------------------------------SCOPING---------------------------------------
--------------------------------------------------------------------------------
-- "Scoping" is a term I use to refer to
-- (1) building the SymbolTableTree, which involves attaching a symbol table to
-- each node in the AST that notes which variables are visible at that point in the program
-- and
-- (2) checking for scoping errors at the same time

-- builds the initial symbol table that every node has access to, AKA the global scope
buildGlobalScope :: [DeclAug SpanRec] -> SymTable
buildGlobalScope ds =
  let addIfAbsent :: DeclAug SpanRec -> SymTable -> SymTable
      addIfAbsent entry tbl =
        case Map.lookup (getDeclSymbol entry) tbl of
          Just _ ->
            astSemanticErr
              entry
              ( "symbol "
                  ++ getDeclSymbol entry
                  ++ " has already been declared in global scope"
              )
          Nothing -> insertDecl entry tbl
      -- verifies that main exists
      -- if success, equivalent to `id`
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

-- performs the "scoping" part of validation for declarations
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
buildSymTreeD tbl c@(CallDecl {}, _) = buildSymTreeCall tbl c

-- performs the "scoping" part of validation for expressions
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
-- practically identical to CallDecl
buildSymTreeE tbl c@(CallExpr {}, _) = buildSymTreeCall tbl c
buildSymTreeE tbl i@(IfExpr b fe eis e, sr) =
  let buildSymTreeEF symtbl (eib, eie) =
        (buildSymTreeE symtbl eib, buildSymTreeE symtbl eie)
   in ( IfExpr
          (buildSymTreeE tbl b)
          (buildSymTreeE tbl fe)
          (map (buildSymTreeEF tbl) eis)
          (buildSymTreeE tbl e),
        SymData tbl sr
      )
---------------------------SCOPING OPERATOR EXPRS-------------------------------
buildSymTreeE tbl b@(BinOp op l r, sr) =
  (BinOp op (buildSymTreeE tbl l) (buildSymTreeE tbl r), SymData tbl sr)
buildSymTreeE tbl u@(UnaryOp op t, sr) =
  (UnaryOp op (buildSymTreeE tbl t), SymData tbl sr)

--------------------------------------------------------------------------------
----------------------------------TYPECHECKING----------------------------------
--------------------------------------------------------------------------------
-- these functions have the signature Node -> Node
-- They should act identical to `id` if typechecking succeeds, otherwise they
-- throw errors
--
-- performs the typechecking part of validation for declarations
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
typecheckD c@(CallDecl {}, _) = typecheckCall c

-- helper functions for typechecking expressions
numericalTypes = ["Int", "Float"] -- for arithmetic/relational exprs

-- ensures that the term t is a Bool
handleBTerm :: ExprAug SymData -> ExprAug SymData -> ExprAug SymData
handleBTerm t parent =
  let tType = getType t
   in if tType /= "Bool"
        then typeError parent "Bool" t tType
        else t

-- ensures the term t is a numericalType
handleATerm :: ExprAug SymData -> ExprAug SymData -> ExprAug SymData
handleATerm t parent =
  let tType = getType t
   in if tType `notElem` numericalTypes
        then typeError parent "Float or Int" t tType
        else t

-- performs the typechecking part of validation for expressions
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
typecheckE c@(CallExpr {}, _) = typecheckCall c
typecheckE i@(IfExpr b fe eis e, s) =
  let checkCondType :: ExprAug SymData -> ExprAug SymData
      -- first: need to ensure that each condition is a boolean
      checkCondType cond =
        let condType = getType cond
         in if condType == "Bool"
              then cond
              else typeError i "Bool" b condType
      -- second: need to unify all types of branch expressions
      unifyBranches :: [ExprAug SymData] -> [ExprAug SymData]
      unifyBranches bs =
        let checkBranch :: Type -> ExprAug SymData -> ExprAug SymData
            checkBranch t ex =
              let branchType = getType ex
               in if getType ex == t
                    then ex
                    else typeError fe t ex branchType
         in map (checkBranch $ getType fe) bs
      unifiedBranches = unifyBranches (fe : map snd eis ++ [e])
   in ( IfExpr
          (typecheckE $ checkCondType b)
          (typecheckE $ head unifiedBranches)
          ( zipWith
              ( curry
                  ( uncurry
                      bimap
                      (typecheckE . checkCondType, typecheckE)
                  )
              )
              (map fst eis)
              (tail $ init unifiedBranches)
          )
          (typecheckE $ last unifiedBranches),
        s
      )
--------------------------TYPECHECKING BOOLEAN EXPRS----------------------------
typecheckE b@(BinOp And l r, s) =
  ( BinOp
      And
      (typecheckE $ handleBTerm l b)
      (typecheckE $ handleBTerm r b),
    s
  )
typecheckE b@(BinOp Or l r, s) =
  ( BinOp
      Or
      (typecheckE $ handleBTerm l b)
      (typecheckE $ handleBTerm r b),
    s
  )
typecheckE b@(UnaryOp Not t, s) =
  ( UnaryOp
      Not
      (typecheckE $ handleBTerm t b),
    s
  )

------------------------TYPECHECKING ARITH/REL EXPRS-------------------------------
-- no longer need to pmatch on type of op since all remaining ops take numericalTypes
typecheckE e@(BinOp op l r, s) =
  ( BinOp
      op
      (typecheckE $ handleATerm l e)
      (typecheckE $ handleATerm r e),
    s
  )
typecheckE e@(UnaryOp op t, s) =
  ( UnaryOp
      op
      (typecheckE $ handleATerm t e),
    s
  )

-- special function only for expressions to determine which Type they result in
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
-- getType IfExpr is consistent with typecheckE IfExpr
-- in that func, we verified that all branches have the same type as the first,
-- so we only need to return the first
getType (IfExpr _ fe _ _, _) = getType fe
-- boolean operations
getType (BinOp And _ _, _) = "Bool"
getType (BinOp Or _ _, _) = "Bool"
getType (UnaryOp Not _, _) = "Bool"
-- relational operations
getType (BinOp Less _ _, _) = "Bool"
getType (BinOp Leq _ _, _) = "Bool"
getType (BinOp Greater _ _, _) = "Bool"
getType (BinOp Geq _ _, _) = "Bool"
getType (BinOp Equal _ _, _) = "Bool"
getType (BinOp NotEqual _ _, _) = "Bool"
-- arithmetic operations
-- each term has been verified to have the same type (either Int or Float)
-- by the typechecking step, so getting the type of the first term is enough
getType (BinOp Plus f _, _) = getType f
getType (BinOp Minus f _, _) = getType f
getType (BinOp Mul f _, _) = getType f
getType (BinOp Divide f _, _) = getType f
getType (BinOp IntDivide f _, _) = "Int"
getType (UnaryOp Neg e, _) = getType e

-- sharing functionality between CallExpr and CallDecl because their semantics
-- are identical
-- NOTE: maybe need to do this for Blocks as well in the future
class Call t where
  getId :: t a -> Identifier
  getPExprs :: t a -> [ExprAug a]
  newCall :: Identifier -> [ExprAug a] -> t a

instance Call Decl where
  getId (CallDecl i _) = i
  getId _ = undefined
  getPExprs (CallDecl _ ps) = ps
  getPExprs _ = undefined
  newCall = CallDecl

instance Call Expr where
  getId (CallExpr i _) = i
  getId _ = undefined
  getPExprs (CallExpr _ ps) = ps
  getPExprs _ = undefined
  newCall = CallExpr

buildSymTreeCall ::
  (Call t, ErrRep (t SpanRec)) =>
  SymTable ->
  (t SpanRec, SpanRec) ->
  (t SymData, SymData)
buildSymTreeCall tbl c@(cinst, sr) =
  case Map.lookup (getId cinst) tbl of
    Nothing -> astSemanticErr c ("undeclared function " ++ getId cinst)
    Just (Single _) ->
      astSemanticErr
        c
        ( "attempt to call a variable "
            ++ getId cinst
            ++ " like a function"
        )
    Just _ ->
      (newCall (getId cinst) (map (buildSymTreeE tbl) (getPExprs cinst)), SymData tbl sr)

typecheckCall ::
  (Call t, ErrRep (t SymData)) =>
  (t SymData, SymData) ->
  (t SymData, SymData)
typecheckCall c@(cinst, SymData tbl sr) =
  let i = getId cinst
      pexprs = getPExprs cinst
      callEntry = tbl Map.! i
      getParamTypes (Many ts t) = ts
      pTypes = getParamTypes callEntry
      matchParamType :: ExprAug SymData -> Type -> ExprAug SymData
      matchParamType expr t =
        let passedType = getType expr
         in if passedType == t
              then expr
              else typeError c t expr passedType
   in if length pTypes /= length pexprs
        then
          astSemanticErr
            (cinst, sr)
            ( "passed "
                ++ show (length pexprs)
                ++ " parameter(s) to a function that takes "
                ++ show (length pTypes)
                ++ " parameter(s)"
            )
        else (newCall i (zipWith matchParamType pexprs pTypes), SymData tbl sr)
