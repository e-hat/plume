module Ir.ThreeAddressCode.Translation (toTac) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Ir.ThreeAddressCode.Types
import qualified Parsing.Syntax as S
import Semantics.SymbolTable
import qualified Semantics.Validation as V

toTac :: SymTreeList -> Program
toTac (SymTreeList topLevelStmts) =
    let ds = map getSymStmtAug topLevelStmts
        globals = collectGlobals ds
        funcStmts = filter isFunc ds
        funcs = map (func globals) funcStmts
     in Program $ M.fromList $ zip (map getFuncName funcStmts) funcs

type Env = M.Map S.Identifier Symbol

collectGlobals :: [S.StmtAug SymData] -> Env
collectGlobals = fst . foldl step (M.empty, 0)
  where
    step :: (Env, Int) -> S.StmtAug SymData -> (Env, Int)
    step (mapping, counter) (S.Let typ sym _, _) =
        (M.insert sym (Global counter typ) mapping, counter + 1)
    step st _ = st

getFuncName :: S.StmtAug SymData -> S.Identifier
getFuncName (S.DefFn name _ _ _, _) = name
getFuncName _ = error "expected a function statement"

isFunc :: S.StmtAug SymData -> Bool
isFunc (S.DefFn{}, _) = True
isFunc _ = False

data TState = TState
    { getCurrentFunc :: Func
    , getLocalCounter :: Int
    , getEnv :: Env
    }

setCurrentFunc :: Func -> State TState ()
setCurrentFunc f = modify $ \s -> s{getCurrentFunc = f}

setEnv :: Env -> State TState ()
setEnv env = modify $ \s -> s{getEnv = env}

setLocalCounter :: Int -> State TState ()
setLocalCounter n = modify $ \s -> s{getLocalCounter = n}

addVarToEnv :: S.Identifier -> Symbol -> State TState ()
addVarToEnv name sym = do
    env <- gets getEnv
    setEnv $ M.insert name sym env

lookupVar :: S.Identifier -> State TState Symbol
lookupVar name = do
    env <- gets getEnv
    return $ env M.! name

appendInst :: Inst -> State TState ()
appendInst i = do
    f <- gets getCurrentFunc
    setCurrentFunc $ f{getFunc = getFunc f ++ [i]}

nextLocal :: Type -> State TState Symbol
nextLocal typ = do
    n <- gets getLocalCounter
    setLocalCounter (n + 1)
    return $ Local n typ

paramMap :: [S.Param] -> Env
paramMap = fst . foldl step (M.empty, 0)
  where
    step :: (Env, Int) -> S.Param -> (Env, Int)
    step (m, n) (S.Param (typ, i)) =
        (M.insert i (Param n typ) m, n + 1)

-- Translation functions
-- The names of these functions do not have the word "translate" preceding them
-- because it was becoming repetitive
func :: Env -> S.StmtAug SymData -> Func
func globals funcDef@(S.DefFn _ ps ret _, _) =
    let paramTypes = map (fst . S.getParam) ps
        initial = TState (Func paramTypes ret []) 0 (M.union globals (paramMap ps))
        final = execState (stmt funcDef) initial
     in getCurrentFunc final
func _ _ = error "expected a function statement"

stmt :: S.StmtAug SymData -> State TState ()
stmt (S.DefFn _ _ "Void" e, _) = do
    voidExpr e
    appendInst $ Return Nothing
stmt (S.DefFn _ _ _ e, _) = do
    result <- exprExpr e
    appendInst $ Return $ Just result
stmt (S.Let typ name e, _) = do
    rhs <- exprExpr e
    lhs <- nextLocal typ
    addVarToEnv name lhs
    appendInst $ assignment lhs rhs
stmt (S.Reassign name e, _) = do
    rhs <- exprExpr e
    lhs <- lookupVar name
    appendInst $ assignment lhs rhs
stmt (S.CallStmt name paramExprs, _) = do
    paramTerms <- mapM exprTerm paramExprs
    appendInst $ IgnoreReturnValCall $ FuncCall (name, paramTerms)
stmt (S.BlockStmt stmts, _) = do
    body <- snd <$> translateSublist (mapM_ stmt stmts)
    appendInst $ Block body
stmt (S.IfStmt predicate cons [] mElse, _) = do
    predExpr <- exprExpr predicate
    consList <- snd <$> translateSublist (stmt cons)
    alternative <-
        case mElse of
            Nothing -> return []
            Just d -> snd <$> translateSublist (stmt d)
    appendInst $ Cond predExpr consList alternative
stmt (S.IfStmt predicate cons ((elseIfPred, elseIfCons) : elseIfs) mElse, symData) = do
    predExpr <- exprExpr predicate
    consList <- snd <$> translateSublist (stmt cons)
    alternative <- snd <$> translateSublist (stmt (S.IfStmt elseIfPred elseIfCons elseIfs mElse, symData))
    appendInst $ Cond predExpr consList alternative
stmt (S.WhileStmt cond body, _) = do
    condExpr <- exprExpr cond
    bodyList <- snd <$> translateSublist (stmt body)
    appendInst $ While condExpr bodyList

exprExpr :: S.ExprAug SymData -> State TState (Expr Term)
exprExpr e@(S.Subs{}, _) = None <$> exprTerm e
exprExpr (S.CallExpr name paramExprs, SymData tbl _) = do
    paramTerms <- mapM exprTerm paramExprs
    return (FuncCallExpr (lookupSymbolType name tbl) (FuncCall (name, paramTerms)))
exprExpr e@(S.IfExpr{}, _) = None <$> exprTerm e
exprExpr e@(S.BlockExpr{}, _) = None <$> exprTerm e
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
exprExpr (S.Return, _) = error "only `voidExpr` should be called on Return ExprAug"

exprTerm :: S.ExprAug SymData -> State TState Term
exprTerm (S.Subs name, _) = do
    env <- gets getEnv
    return $ Subs $ env M.! name
exprTerm e@(S.CallExpr name _, SymData tbl _) = do
    rhs <- exprExpr e
    -- lookup return type of function `name`
    lhs <- nextLocal (lookupSymbolType name tbl)
    appendInst $ assignment lhs rhs
    return $ Subs lhs
exprTerm (S.BlockExpr stmts out, _) = do
    (result, block) <- translateSublist (mapM_ stmt stmts >> exprTerm out)
    appendInst $ Block block
    return result
exprTerm e@(S.IfExpr{}, _) = do
    dst <- nextLocal $ V.getType e
    ifExprTermHelper dst e
    return $ Subs dst
exprTerm (S.LitInt l, _) = return $ LitInt l
exprTerm (S.LitFloat l, _) = return $ LitFloat l
exprTerm (S.LitString l, _) = return $ LitString l
exprTerm (S.LitBool l, _) = return $ LitBool l
exprTerm (S.LitChar l, _) = return $ LitChar l
exprTerm e = do
    rhs <- exprExpr e
    lhs <- nextLocal (getType rhs)
    appendInst $ assignment lhs rhs
    return $ Subs lhs

ifExprTermHelper :: Symbol -> S.ExprAug SymData -> State TState ()
ifExprTermHelper dst (S.IfExpr predicate cons [] els, _) = do
    predExpr <- exprExpr predicate
    (consExpr, consList) <- translateSublist (exprExpr cons)
    (elseExpr, elseList) <- translateSublist (exprExpr els)
    appendInst $
        Cond
            predExpr
            (consList ++ [assignment dst consExpr])
            (elseList ++ [assignment dst elseExpr])
ifExprTermHelper dst (S.IfExpr predicate cons ((elseIfPred, elseIfCons) : elseIfs) els, symData) = do
    predExpr <- exprExpr predicate
    (consExpr, consList) <- translateSublist (exprExpr cons)
    alternative <- snd <$> translateSublist (ifExprTermHelper dst (S.IfExpr elseIfPred elseIfCons elseIfs els, symData))
    appendInst $
        Cond
            predExpr
            (consList ++ [assignment dst consExpr])
            alternative
ifExprTermHelper _ _ = undefined

-- Translates a term and returns the list of instructions that the term creates,
-- without changing the State. It also returns intermediate State used for the dummy translation.
translateSublist :: State TState a -> State TState (a, [Inst])
translateSublist action = do
    before <- get
    let initial = before{getCurrentFunc = Func [] [] []}
    let (result, final) = runState action initial
    put $ before{getLocalCounter = getLocalCounter final}
    return (result, getFunc $ getCurrentFunc final)

-- These are the only Expr's that could possibly return void
voidExpr :: S.ExprAug SymData -> State TState ()
voidExpr node@(S.CallExpr{}, _) = do
    funcCallExpr <- exprExpr node
    case funcCallExpr of
        (FuncCallExpr _ call) -> do
            appendInst $ IgnoreReturnValCall call
            return ()
        _ -> undefined
voidExpr (S.Return, _) = return ()
voidExpr node = do
    _ <- exprExpr node
    return ()

assignment :: Symbol -> Expr Term -> Inst
assignment lhs = Assignment (None (Subs lhs))
