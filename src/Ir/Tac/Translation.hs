module Ir.Tac.Translation (toTac) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Ir.Tac.Types
import qualified Parsing.Syntax as S
import Semantics.SymbolTable
import qualified Semantics.Validation as V

toTac :: SymTreeList -> Program
toTac (SymTreeList topLevelDecls) =
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
    }

setCurrentFunc :: Func -> State Translator ()
setCurrentFunc f = modify $ \s -> s{getCurrentFunc = f}

setEnv :: Env -> State Translator ()
setEnv env = modify $ \s -> s{getEnv = env}

setLocalCounter :: Int -> State Translator ()
setLocalCounter n = modify $ \s -> s{getLocalCounter = n}

addVarToEnv :: S.Identifier -> Symbol -> State Translator ()
addVarToEnv name sym = do
    env <- gets getEnv
    setEnv $ M.insert name sym env

lookupVar :: S.Identifier -> State Translator Symbol
lookupVar name = do
    env <- gets getEnv
    return $ env M.! name

appendInst :: Inst -> State Translator ()
appendInst i = do
    f <- gets getCurrentFunc
    setCurrentFunc $ f{getFunc = getFunc f ++ [i]}

nextLocal :: Type -> State Translator Symbol
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
func :: Env -> S.DeclAug SymData -> Func
func globals funcDef@(S.DefFn _ ps ret _, _) =
    let paramTypes = map (fst . S.getParam) ps
        initial = Translator (Func paramTypes ret []) 0 (M.union globals (paramMap ps))
        final = execState (decl funcDef) initial
     in getCurrentFunc final
func _ _ = error "expected a function declaration"

decl :: S.DeclAug SymData -> State Translator ()
decl (S.DefFn _ _ "Void" e, _) = do
    voidExpr e
    appendInst $ Return Nothing
decl (S.DefFn _ _ _ e, _) = do
    result <- exprExpr e
    appendInst $ Return $ Just result
decl (S.Let typ name e, _) = do
    rhs <- exprExpr e
    lhs <- nextLocal typ
    addVarToEnv name lhs
    appendInst $ assignment lhs rhs
decl (S.Reassign name e, _) = do
    rhs <- exprExpr e
    lhs <- lookupVar name
    appendInst $ assignment lhs rhs
decl (S.CallDecl name paramExprs, _) = do
    paramTerms <- mapM exprTerm paramExprs
    appendInst $ IgnoreReturnValCall $ FuncCall (name, paramTerms)
decl (S.BlockDecl decls, _) = do
    body <- snd <$> translateSublist (mapM_ decl decls)
    appendInst $ Block body
decl (S.IfDecl predicate cons [] mElse, _) = do
    predExpr <- exprExpr predicate
    consList <- snd <$> translateSublist (decl cons)
    alternative <-
        case mElse of
            Nothing -> return []
            Just d -> snd <$> translateSublist (decl d)
    appendInst $ Cond predExpr consList alternative
decl (S.IfDecl predicate cons ((elseIfPred, elseIfCons) : elseIfs) mElse, symData) = do
    predExpr <- exprExpr predicate
    consList <- snd <$> translateSublist (decl cons)
    alternative <- snd <$> translateSublist (decl (S.IfDecl elseIfPred elseIfCons elseIfs mElse, symData))
    appendInst $ Cond predExpr consList alternative
decl (S.WhileDecl cond body, _) = do
    condExpr <- exprExpr cond
    bodyList <- snd <$> translateSublist (decl body)
    appendInst $ While condExpr bodyList

exprExpr :: S.ExprAug SymData -> State Translator (Expr Term)
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

exprTerm :: S.ExprAug SymData -> State Translator Term
exprTerm (S.Subs name, _) = do
    env <- gets getEnv
    return $ Subs $ env M.! name
exprTerm e@(S.CallExpr name _, SymData tbl _) = do
    rhs <- exprExpr e
    -- lookup return type of function `name`
    lhs <- nextLocal (lookupSymbolType name tbl)
    appendInst $ assignment lhs rhs
    return $ Subs lhs
exprTerm (S.BlockExpr decls out, _) = do
    (result, block) <- translateSublist (mapM_ decl decls >> exprTerm out)
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

ifExprTermHelper :: Symbol -> S.ExprAug SymData -> State Translator ()
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
translateSublist :: State Translator a -> State Translator (a, [Inst])
translateSublist action = do
    before <- get
    let initial = before{getCurrentFunc = Func [] [] []}
    let (result, final) = runState action initial
    put $ before{getLocalCounter = getLocalCounter final}
    return (result, getFunc $ getCurrentFunc final)

-- These are the only Expr's that could possibly return void
voidExpr :: S.ExprAug SymData -> State Translator ()
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
