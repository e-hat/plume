module Semantics (validateSemantics) where

import Control.Monad
import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

-- this function performs scoping (symbol table building + catching scoping errors)
-- & typechecking for a given AST
validateSemantics :: Program -> Either String SymTreeList
validateSemantics p =
  let checkGlobalLet :: DeclAug SpanRec -> Either String (DeclAug SpanRec)
      checkGlobalLet l@(Let _ _ e, sr) =
        if isLit e
          then Right l
          else Left $ astSemanticErr l "a global let MUST be a literal value"
      checkGlobalLet d = Right d
      -- handling let expressions differently since global scope has already been created
      globalSymTreeD :: SymTable -> DeclAug SpanRec -> Either String (DeclAug SymData)
      globalSymTreeD tbl (Let t i e, sr) = do 
        bl <- Let t i <$> buildSymTreeE tbl e 
        return (bl, SymData tbl sr)
      globalSymTreeD tbl d = buildSymTreeD tbl d
   in do
        let globals = map getASTDeclAug (getProgram p)
        globalScope <- buildGlobalScope globals
        globalDecls <- traverse checkGlobalLet globals
        symTrees <- 
          traverse (globalSymTreeD globalScope >=> typecheckD) globalDecls
        return $ SymTreeList $ map SymDeclAug symTrees

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
buildGlobalScope :: [DeclAug SpanRec] -> Either String SymTable
buildGlobalScope ds =
  let addIfAbsent :: SymTable -> DeclAug SpanRec -> Either String SymTable
      addIfAbsent tbl entry =
        case Map.lookup (getDeclSymbol entry) tbl of
          Just _ ->
            Left $
              astSemanticErr
                entry
                ( "symbol "
                    ++ getDeclSymbol entry
                    ++ " has already been declared in global scope"
                )
          Nothing -> Right $ insertDecl entry tbl
      -- verifies that main exists
      -- if success, equivalent to `id`
      checkMain :: SymTable -> Either String SymTable
      checkMain tbl =
        case Map.lookup "main" tbl of
          Just (Single _) -> Left "main MUST be a function, not a variable"
          Just m ->
            if m == Many [] "Int"
              then Right tbl
              else Left "wrong type for main function, expected main(): Int"
          Nothing -> Left "missing declaration of main function"
   in do
        globalScope <- foldM addIfAbsent Map.empty ds
        checkMain globalScope

-- shared btwn BlockDecl and BlockExpr for accumulating symbol tables
buildBlockTbls :: [SymTable] -> DeclAug t -> [SymTable]
buildBlockTbls tbls l@(Let {}, _) =
  let t = last tbls
   in tbls ++ [insertDecl l t]
buildBlockTbls tbls _ = tbls ++ [last tbls]

-- performs the "scoping" part of validation for declarations
buildSymTreeD :: SymTable -> DeclAug SpanRec -> Either String (DeclAug SymData)
buildSymTreeD tbl l@(Let t i e, sr) =
  case Map.lookup (getDeclSymbol l) tbl of
    Just _ -> Left $ astSemanticErr l ("overlapping symbol " ++ i)
    Nothing -> do
      syml <- Let t i <$> buildSymTreeE tbl e
      return (syml, SymData tbl sr)
buildSymTreeD tbl f@(DefFn i ps t e, sr) =
  let childTbl = foldr insertParam tbl ps
   in do
        symf <- DefFn i ps t <$> buildSymTreeE childTbl e
        return (symf, SymData tbl sr)
buildSymTreeD tbl r@(Reassign i e, sr) =
  case Map.lookup i tbl of
    Nothing -> Left $ astSemanticErr r ("undeclared symbol " ++ i)
    Just _ -> do
      symr <- Reassign i <$> buildSymTreeE tbl e
      return (symr, SymData tbl sr)
buildSymTreeD tbl c@(CallDecl {}, _) = buildSymTreeCall tbl c
buildSymTreeD tbl b@(BlockDecl ds, sr) = do
  let dtbls = foldl buildBlockTbls [tbl] ds
  symds <- zipWithM buildSymTreeD dtbls ds
  return (BlockDecl symds, SymData tbl sr)
buildSymTreeD tbl i@(IfDecl b fd eis med, sr) =
  let buildSymTreeEF symtbl (eib, eid) = 
        (,) <$> buildSymTreeE symtbl eib <*> buildSymTreeD symtbl eid 
   in do
        bi <- IfDecl <$> buildSymTreeE tbl b 
                     <*> buildSymTreeD tbl fd 
                     <*> traverse (buildSymTreeEF tbl) eis 
                     <*> traverse (buildSymTreeD tbl) med 
        return (bi, SymData tbl sr)

-- performs the "scoping" part of validation for expressions
buildSymTreeE :: SymTable -> ExprAug SpanRec -> Either String (ExprAug SymData)
-- Literals are easy
-- By definition, they don't have much to do with the Symbol table
buildSymTreeE tbl (LitInt i, sr) = Right (LitInt i, SymData tbl sr)
buildSymTreeE tbl (LitBool b, sr) = Right (LitBool b, SymData tbl sr)
buildSymTreeE tbl (LitChar c, sr) = Right (LitChar c, SymData tbl sr)
buildSymTreeE tbl (LitString s, sr) = Right (LitString s, SymData tbl sr)
buildSymTreeE tbl (LitFloat f, sr) = Right (LitFloat f, SymData tbl sr)
buildSymTreeE tbl (Return, sr) = Right (Return, SymData tbl sr)
-- BlockExpr will be slightly different, needs to accumulate declared symbols
buildSymTreeE tbl (BlockExpr ds e, sr) = do
  let dtbls = foldl buildBlockTbls [tbl] ds
  symds <- zipWithM buildSymTreeD dtbls ds
  syme <- buildSymTreeE (last dtbls) e
  return (BlockExpr symds syme, SymData tbl sr)
buildSymTreeE tbl s@(Subs i, sr) =
  case Map.lookup i tbl of
    Just _ -> Right (Subs i, SymData tbl sr)
    Nothing -> Left $ astSemanticErr s ("undeclared symbol " ++ i)
-- practically identical to CallDecl
buildSymTreeE tbl c@(CallExpr {}, _) = buildSymTreeCall tbl c
buildSymTreeE tbl i@(IfExpr b fe eis e, sr) =
  let buildSymTreeEF symtbl (eib, eie) =
        (,) <$> buildSymTreeE symtbl eib <*> buildSymTreeE symtbl eie
   in do
     symi <- IfExpr <$> buildSymTreeE tbl b 
                    <*> buildSymTreeE tbl fe 
                    <*> traverse (buildSymTreeEF tbl) eis 
                    <*> buildSymTreeE tbl e
     return (symi, SymData tbl sr)
---------------------------SCOPING OPERATOR EXPRS-------------------------------
buildSymTreeE tbl b@(BinOp op l r, sr) = do
  bb <- BinOp op <$> buildSymTreeE tbl l <*> buildSymTreeE tbl r
  return (bb, SymData tbl sr)
buildSymTreeE tbl u@(UnaryOp op t, sr) = do
  bu <- UnaryOp op <$> buildSymTreeE tbl t
  return (bu, SymData tbl sr)

--------------------------------------------------------------------------------
----------------------------------TYPECHECKING----------------------------------
--------------------------------------------------------------------------------
-- these functions have the signature Node -> Node
-- They should act identical to `id` if typechecking succeeds, otherwise they
-- throw errors

-- performs the typechecking part of validation for declarations
typecheckD :: DeclAug SymData -> Either String (DeclAug SymData)
typecheckD l@(Let t i e, s) =
  let t1 = getDeclType l
      t2 = getType e
   in if t1 == t2
        then do
          tl <- Let t i <$> typecheckE e
          return (tl, s)
        else Left $ typeError l t1 e t2
typecheckD f@(DefFn i ps rt e, s) =
  let t1 = getDeclType f
      t2 = getType e
   in if t1 == t2
        then do
          td <- DefFn i ps rt <$> typecheckE e
          return (td, s)
        else Left $ typeError f t1 e t2
typecheckD r@(Reassign i e, s@(SymData tbl _)) =
  let t1 = lookupSymbolType i tbl
      t2 = getType e
   in if t1 == t2
        then do
          tr <- Reassign i <$> typecheckE e
          return (tr, s)
        else Left $ typeError r t1 e t2
typecheckD c@(CallDecl {}, _) = typecheckCall c
typecheckD b@(BlockDecl ds, s) = do
  tds <- traverse typecheckD ds
  return (BlockDecl tds, s)
typecheckD i@(IfDecl b fd eis med, s) =
  let applyTplM (f, g) (x, y) = (,) <$> f x <*> g y
   in do 
     tb <- handleBTerm i b >>= typecheckE 
     tfd <- typecheckD fd 
     teisi <- traverse (applyTplM (handleBTerm i, typecheckD)) eis
     teisf <- traverse (applyTplM (typecheckE, pure)) teisi
     tmed <- traverse typecheckD med
     return (IfDecl tb tfd teisf tmed, s)

-- helper functions for typechecking expressions
numericalTypes = ["Int", "Float"] -- for arithmetic/relational exprs

-- ensures that the term t is a Bool
handleBTerm ::
  (ErrRep a) =>
  (a, SymData) ->
  ExprAug SymData ->
  Either String (ExprAug SymData)
handleBTerm parent t =
  let tType = getType t
   in if tType /= "Bool"
        then Left $ typeError parent "Bool" t tType
        else Right t

-- ensures the term t is a numericalType
handleATerm ::
  ExprAug SymData -> ExprAug SymData -> Either String (ExprAug SymData)
handleATerm parent t =
  let tType = getType t
   in if tType `notElem` numericalTypes
        then Left $ typeError parent "Float or Int" t tType
        else Right t

-- performs the typechecking part of validation for expressions
typecheckE :: ExprAug SymData -> Either String (ExprAug SymData)
typecheckE i@(LitInt _, _) = Right i
typecheckE s@(LitString _, _) = Right s
typecheckE f@(LitFloat _, _) = Right f
typecheckE c@(LitChar _, _) = Right c
typecheckE b@(LitBool _, _) = Right b
typecheckE r@(Return, _) = Right r
typecheckE (BlockExpr ds rexpr, s) = do
  tds <- traverse typecheckD ds
  trexpr <- typecheckE rexpr
  return (BlockExpr tds trexpr, s)
typecheckE s@(Subs _, _) = Right s
typecheckE c@(CallExpr {}, _) = typecheckCall c
typecheckE i@(IfExpr b fe eis e, s) =
  let -- need to unify all types of branch expressions
      unifyBranches :: [ExprAug SymData] -> Either String [ExprAug SymData]
      unifyBranches bs =
        let checkBranch :: Type -> ExprAug SymData -> Either String (ExprAug SymData)
            checkBranch t ex =
              let branchType = getType ex
               in if getType ex == t
                    then Right ex
                    else Left $ typeError fe t ex branchType
         in traverse (checkBranch $ getType fe) bs
      combineEFs :: ExprAug SymData -> ExprAug SymData -> Either String (ExprAug SymData, ExprAug SymData)
      combineEFs cond branch =
        (,) <$> (handleBTerm i cond >>= typecheckE) <*> typecheckE branch
   in do
        tb <- handleBTerm i b >>= typecheckE
        unifiedBranches <- unifyBranches (fe : map snd eis ++ [e])
        tfe <- typecheckE $ head unifiedBranches
        teis <- zipWithM combineEFs (map fst eis) (tail $ init unifiedBranches)
        te <- typecheckE (last unifiedBranches)
        return (IfExpr tb tfe teis te, s)
--------------------------TYPECHECKING BOOLEAN EXPRS----------------------------
typecheckE b@(BinOp And l r, s) = do
  tl <- handleBTerm b l >>= typecheckE
  tr <- handleBTerm b r >>= typecheckE
  return (BinOp And tl tr, s)
typecheckE b@(BinOp Or l r, s) = do
  tl <- handleBTerm b l >>= typecheckE
  tr <- handleBTerm b r >>= typecheckE
  return (BinOp Or tl tr, s)
typecheckE b@(UnaryOp Not t, s) = do
  tt <- handleBTerm b t >>= typecheckE
  return (UnaryOp Not tt, s)

------------------------SPECIAL REL. EXPRS--------------------------------------
typecheckE eq@(BinOp Equal l r, s) = 
  let lt = getType l 
      rt = getType r 
   in if lt /= rt
     then Left $ typeError l lt r rt 
     else do 
       teq <- BinOp Equal <$> typecheckE l <*> typecheckE r 
       return (teq, s)
typecheckE eq@(BinOp NotEqual l r, s) = 
  let lt = getType l 
      rt = getType r 
   in if lt /= rt
     then Left $ typeError l lt r rt 
     else do 
       teq <- BinOp NotEqual <$> typecheckE l <*> typecheckE r 
       return (teq, s)
------------------------TYPECHECKING ARITH/REL EXPRS-------------------------------
-- no longer need to pmatch on type of op since all remaining ops take numericalTypes
typecheckE e@(BinOp op l r, s) = do
  tl <- handleATerm e l >>= typecheckE
  tr <- handleATerm e r >>= typecheckE
  return (BinOp op tl tr, s)
typecheckE e@(UnaryOp op t, s) = do
  tt <- handleATerm e t >>= typecheckE
  return (UnaryOp op tt, s)

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
getType (UnaryOp Neg e, _) = getType e
getType (BinOp _ l r, _) = promoteType (getType l) (getType r)

promoteType :: Type -> Type -> Type 
promoteType "Float" _ = "Float"
promoteType _ "Float" = "Float"
promoteType "Int" "Int" = "Int"

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
  Either String (t SymData, SymData)
buildSymTreeCall tbl c@(cinst, sr) =
  case Map.lookup (getId cinst) tbl of
    Nothing -> Left $ astSemanticErr c ("undeclared function " ++ getId cinst)
    Just (Single _) ->
      Left $
        astSemanticErr
          c
          ( "attempt to call a variable "
              ++ getId cinst
              ++ " like a function"
          )
    Just _ -> do
      symc <- newCall (getId cinst) <$> traverse (buildSymTreeE tbl) (getPExprs cinst)
      return (symc, SymData tbl sr)

typecheckCall ::
  (Call t, ErrRep (t SymData)) =>
  (t SymData, SymData) ->
  Either String (t SymData, SymData)
typecheckCall c@(cinst, SymData tbl sr) =
  let i = getId cinst
      pexprs = getPExprs cinst
      callEntry = tbl Map.! i
      getParamTypes (Many ts t) = ts
      pTypes = getParamTypes callEntry
      matchParamType :: ExprAug SymData -> Type -> Either String (ExprAug SymData)
      matchParamType expr t =
        let passedType = getType expr
         in if passedType == t
              then Right expr
              else Left $ typeError c t expr passedType
   in if length pTypes /= length pexprs
        then
          Left $
            astSemanticErr
              (cinst, sr)
              ( "passed "
                  ++ show (length pexprs)
                  ++ " parameter(s) to a function that takes "
                  ++ show (length pTypes)
                  ++ " parameter(s)"
              )
        else do
          tc <- newCall i <$> zipWithM matchParamType pexprs pTypes
          return (tc, SymData tbl sr)
