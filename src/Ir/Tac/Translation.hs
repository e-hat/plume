module Ir.Tac.Translation (translate) where

import Ir.Tac.Types
import qualified Parsing.Syntax as S
import Semantics.SymbolTable
import qualified Semantics.Validation as V

import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Sq

translate :: SymTreeList -> Program
translate (SymTreeList topLevelDecls) =
  let ds = map getSymDeclAug topLevelDecls
      globals = collectGlobals ds
      funcDecls = filter isFunc ds
      funcs = map (func globals) funcDecls
   in Program $ M.fromList $ zip (map getFuncName funcDecls) funcs

type Env = M.Map S.Identifier Symbol

collectGlobals :: [S.DeclAug SymData] -> Env
collectGlobals = fst . foldl step (M.empty, 0)
 where
  step :: (Env, Int) -> S.DeclAug SymData -> (Env, Int)
  step (mapping, counter) (S.Let typ sym _, _) =
    (M.insert sym (Global counter typ) mapping, counter + 1)
  step st _ = st

getFuncName :: S.DeclAug SymData -> S.Identifier
getFuncName (S.DefFn name _ _ _, _) = name
getFuncName _ = error "expected a function declaration"
isFunc :: S.DeclAug SymData -> Bool
isFunc (S.DefFn{}, _) = True
isFunc _ = False

data Translator = Translator
  { getCurrentFunc :: Func
  , getLocalCounter :: Int
  , getEnv :: Env
  , getEdgeList :: [(Int, Int)]
  }

setCurrentFunc :: Func -> State Translator ()
setCurrentFunc f = modify $ \s -> s{getCurrentFunc = f}

setEnv :: Env -> State Translator ()
setEnv env = modify $ \s -> s{getEnv = env}

setLocalCounter :: Int -> State Translator ()
setLocalCounter n = modify $ \s -> s{getLocalCounter = n}

setEdgeList :: [(Int, Int)] -> State Translator ()
setEdgeList es = modify $ \s -> s{getEdgeList = es}

addEdgeToList :: Int -> Int -> State Translator ()
addEdgeToList src dst = do 
  edgeList <- gets getEdgeList
  setEdgeList $ (src, dst):edgeList

addVarToEnv :: S.Identifier -> Symbol -> State Translator ()
addVarToEnv name sym = do
  env <- gets getEnv
  setEnv $ M.insert name sym env

lookupVar :: S.Identifier -> State Translator Symbol
lookupVar name = do
  env <- gets getEnv
  return $ env M.! name

appendLine :: Line -> State Translator ()
appendLine l = do
  f <- gets getCurrentFunc
  setCurrentFunc $ f{getFunc = getFunc f Sq.|> l}

nextLocal :: Type -> State Translator Symbol
nextLocal typ = do
  n <- gets getLocalCounter
  setLocalCounter (n + 1)
  return $ Local n typ

nextSN :: Translator -> Int
nextSN = length . getFunc . getCurrentFunc

lastSN :: Translator -> Int
lastSN = (+)(-1) . nextSN

addEdgeToFunction :: Func -> (Int, Int) -> Func
addEdgeToFunction f (src, dst) =
  let oldSrc = Sq.index (getFunc f) src
      newSrc = oldSrc{getOutgoing = getOutgoing oldSrc ++ [dst]}
      oldDst = Sq.index (getFunc f) dst
      newDst = oldDst{getIncoming = getIncoming oldSrc ++ [src]}
      afterSrcUpdate = f{getFunc = Sq.update src newSrc (getFunc f)}
   in afterSrcUpdate{getFunc = Sq.update dst newDst (getFunc afterSrcUpdate)}

paramMap :: [S.Param] -> Env
paramMap = fst . foldl step (M.empty, 0)
 where
  step :: (Env, Int) -> S.Param -> (Env, Int)
  step (m, n) (S.Param (typ, i)) =
    (M.insert i (Param n typ) m, n + 1)

-- Translation functions
-- The names of these functions do not have the word "translate" preceding them
-- because it was becoming repetitive
func :: Env -> S.DeclAug SymData -> Func
func globals funcDef@(S.DefFn _ ps _ _, _) =
  let initial = Translator (Func Sq.empty) 0 (M.union globals (paramMap ps)) []
      final = execState (decl funcDef) initial
   in foldl addEdgeToFunction (getCurrentFunc final) (getEdgeList final)
func _ _ = error "expected a function declaration"

decl :: S.DeclAug SymData -> State Translator ()
decl (S.DefFn _ _ "Void" e, _) = do
  voidExpr e
  appendLine (Line (Return Nothing) [] [])
decl (S.DefFn _ _ _ e, _) = do
  result <- exprExpr e
  appendLine (Line (Return (Just result)) [] [])
decl (S.Let typ name e, _) = do
  rhs <- exprExpr e
  lhs <- nextLocal typ
  addVarToEnv name lhs
  appendLine $ Line (Assignment lhs rhs) [] []
decl (S.Reassign name e, _) = do
  rhs <- exprExpr e
  lhs <- lookupVar name
  appendLine $ Line (Assignment lhs rhs) [] []
decl (S.CallDecl name paramExprs, _) = do
  paramTerms <- mapM exprTerm paramExprs
  appendLine $ Line (IgnoreReturnValCall $ FuncCall (name, paramTerms)) [] []
decl (S.BlockDecl decls, _) = mapM_ decl decls
decl ifDecl@(S.IfDecl{}, _) = do
  exitPoints <- ifDeclHelper Nothing ifDecl
  exit <- gets nextSN
  mapM_ (`addEdgeToList` exit) exitPoints

ifDeclHelper :: Maybe Int -> S.DeclAug SymData -> State Translator [Int]
ifDeclHelper mPrevCondSN (S.IfDecl p c [] mElse, _) = do
  -- predStart --> incoming prevCondSN 
  -- pred instructions...
  -- if not (predExpr) --> outgoing elseStart 
  -- cons instructions...
  -- consEnd --> return [consEnd], outgoing elseEnd
  -- elseStart --> incoming condSN
  -- else instructions...
  -- elseEnd --> incoming consEnd
  predExprStart <- gets nextSN
  predExpr <- exprExpr p
  case mPrevCondSN of 
    Just prevCondSN -> addEdgeToList prevCondSN predExprStart
    Nothing -> return ()
  appendLine $ Line (Cond predExpr) [] []
  cond <- gets lastSN
  decl c
  case mElse of 
    Just els -> do 
      consEnd <- gets lastSN
      elseStart <- gets nextSN
      addEdgeToList cond elseStart
      decl els
      return [consEnd]
    Nothing -> return [cond]
ifDeclHelper 
  mPrevCondSN 
  (S.IfDecl p c ((elseIfPred, elseIfCons) : elseIfTail) mElse, s) = do 
  -- predStart --> incoming prevCondSN
  -- pred instructions...
  -- if not (predExpr) --> outgoing elseIfPredStart
  -- cons instructions
  -- consEnd --> return consEnd : (recursion result), outgoing exit
  -- elseIfPredStart --> incoming cond
  predExprStart <- gets nextSN
  predExpr <- exprExpr p
  case mPrevCondSN of 
    Just prevCondSN -> addEdgeToList prevCondSN predExprStart
    Nothing -> return ()
  appendLine $ Line (Cond predExpr) [] []
  cond <- gets lastSN
  decl c
  consEnd <- gets lastSN
  otherEndings <-
    ifDeclHelper 
      (Just cond)
      (S.IfDecl elseIfPred elseIfCons elseIfTail mElse, s)
  return (consEnd : otherEndings)
ifDeclHelper _ _ = undefined

exprExpr :: S.ExprAug SymData -> State Translator (Expr Term)
exprExpr e@(S.Subs{}, _) = None <$> exprTerm e
exprExpr (S.CallExpr name paramExprs, SymData tbl _) = do
  paramTerms <- mapM exprTerm paramExprs
  return (FuncCallExpr (lookupSymbolType name tbl) (FuncCall (name, paramTerms)))
exprExpr e@(S.IfExpr{}, _) = None <$> exprTerm e
exprExpr (S.BlockExpr decls out, _) = do
  mapM_ decl decls
  exprExpr out
exprExpr (S.BinOp op l r, _) = do
  lTerm <- exprTerm l
  rTerm <- exprTerm r
  return $ Bin lTerm op rTerm
exprExpr (S.UnaryOp op e, _) = Un op <$> exprTerm e
exprExpr (S.LitInt int, _) = return $ None $ LitInt (toInteger int)
exprExpr (S.LitFloat float, _) = return $ None $ LitFloat float
exprExpr (S.LitString str, _) = return $ None $ LitString str
exprExpr (S.LitBool bool, _) = return $ None $ LitBool bool
exprExpr (S.LitChar char, _) = return $ None $ LitChar char
exprExpr (S.Return, _) = error "voidExpr should be called on Return ExprAug"

exprTerm :: S.ExprAug SymData -> State Translator Term
exprTerm (S.Subs name, _) = do
  env <- gets getEnv
  return $ Subs $ env M.! name
exprTerm e@(S.CallExpr name _, SymData tbl _) = do
  rhs <- exprExpr e
  -- lookup return type of function `name`
  lhs <- nextLocal (lookupSymbolType name tbl)
  appendLine $ Line (Assignment lhs rhs) [] []
  return $ Subs lhs
exprTerm ifExpr@(S.IfExpr{}, _) = do
  result <- nextLocal (V.getType ifExpr)
  exitPoints <- ifExprTermHelper result Nothing ifExpr
  exit <- gets nextSN
  mapM_ (`addEdgeToList` exit) exitPoints
  return $ Subs result
exprTerm (S.BlockExpr decls out, _) = do
  mapM_ decl decls
  exprTerm out
exprTerm (S.LitInt l, _) = return $ LitInt l
exprTerm (S.LitFloat l, _) = return $ LitFloat l
exprTerm (S.LitString l, _) = return $ LitString l
exprTerm (S.LitBool l, _) = return $ LitBool l
exprTerm (S.LitChar l, _) = return $ LitChar l
exprTerm e = do
  rhs <- exprExpr e
  lhs <- nextLocal (getType rhs)
  appendLine $ Line (Assignment lhs rhs) [] []
  return $ Subs lhs

ifExprTermHelper :: Symbol -> Maybe Int -> S.ExprAug SymData -> State Translator [Int]
ifExprTermHelper result mPrevCondSN (S.IfExpr p c [] els, _) = do
  -- predStart --> incoming prevCondSN
  -- pred instructions...
  -- if not (predExpr) --> outgoing elseStart
  -- cons instructions...
  -- consEnd --> return [consEnd], outgoing elseEnd
  -- elseStart --> incoming condSN
  -- else instructions...
  -- elseEnd --> incoming consEnd
  predExprStart <- gets nextSN
  predExpr <- exprExpr p
  case mPrevCondSN of
    Just prevCondSN -> addEdgeToList prevCondSN predExprStart
    Nothing -> return ()
  appendLine $ Line (Cond predExpr) [] []
  cond <- gets lastSN
  consExpr <- exprExpr c
  appendLine $ Line (Assignment result consExpr) [] []
  consEnd <- gets lastSN
  elseStart <- gets nextSN
  addEdgeToList cond elseStart
  elseExpr <- exprExpr els
  appendLine $ Line (Assignment result elseExpr) [] []
  return [consEnd]
ifExprTermHelper
  result
  mPrevCondSN
  (S.IfExpr p c ((elseIfPred, elseIfCons) : elseIfTail) els, s) = do
    -- predStart --> incoming prevCondSN
    -- pred instructions...
    -- if not (predExpr) --> outgoing elseIfPredStart
    -- cons instructions
    -- consEnd --> return consEnd : (recursion result), outgoing exit
    -- elseIfPredStart --> incoming cond
    predExprStart <- gets nextSN
    predExpr <- exprExpr p
    case mPrevCondSN of
      Just prevCondSN -> addEdgeToList prevCondSN predExprStart
      Nothing -> return ()
    appendLine $ Line (Cond predExpr) [] []
    cond <- gets lastSN
    consExpr <- exprExpr c
    appendLine $ Line (Assignment result consExpr) [] []
    consEnd <- gets lastSN
    otherEndings <-
      ifExprTermHelper
        result
        (Just cond)
        (S.IfExpr elseIfPred elseIfCons elseIfTail els, s)
    return (consEnd : otherEndings)
ifExprTermHelper _ _ _ = error "Expected an IfExpr"

-- These are the only Expr's that could possibly return void
voidExpr :: S.ExprAug SymData -> State Translator ()
voidExpr (S.CallExpr{}, _) = undefined
voidExpr (S.IfExpr{}, _) = undefined
voidExpr (S.BlockExpr{}, _) = undefined
voidExpr (S.Return, _) = return ()
voidExpr _ = error "Cannot use voidExpr with non-void expression"
