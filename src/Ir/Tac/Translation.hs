module Ir.Tac.Translation (translate) where

import Ir.Tac.Types
import qualified Parsing.Syntax as S

import qualified Data.Map.Strict as M
import Control.Monad.State

translate :: S.Program -> Program
translate (S.Program topLevelDecls) =
  let ds = map S.getASTDeclAug topLevelDecls
      globals = collectGlobals ds
      funcDecls = filter isFunc ds
      funcs = map (translateFunc globals) funcDecls
   in Program $ M.fromList $ zip (map getFuncName funcDecls) funcs

getFuncName :: S.DeclAug a -> S.Identifier
getFuncName (S.DefFn name _ _ _, _) = name
getFuncName _ = error "expected a function declaration"

isFunc :: S.DeclAug a -> Bool
isFunc (S.DefFn{}, _) = True
isFunc _ = False

data Translator = Translator 
  { getCurrentFunc :: Func 
  , getLocalCounter :: Int 
  , getEnv :: M.Map S.Identifier Symbol 
  }

setCurrentFunc :: Func -> State Translator ()
setCurrentFunc f = modify $ \s -> s{getCurrentFunc = f}

setEnv :: M.Map S.Identifier Symbol -> State Translator ()
setEnv env = modify $ \s -> s{getEnv = env}

setLocalCounter :: Int -> State Translator ()
setLocalCounter n = modify $ \s -> s{getLocalCounter = n}

addVarToEnv :: S.Identifier -> Symbol -> State Translator ()
addVarToEnv name sym = do 
  env <- gets getEnv
  setEnv $ M.insert name sym env

appendInst :: Maybe Label -> Inst -> State Translator ()
appendInst mlabel i = do 
  f <- gets getCurrentFunc 
  setCurrentFunc $ f{getFunc = getFunc f ++ [(mlabel, i)]}

nextLocal :: State Translator Int 
nextLocal = do 
  n <- gets getLocalCounter 
  setLocalCounter (n + 1)
  return n

paramMap :: [S.Param] -> M.Map S.Identifier Symbol 
paramMap = fst . foldl step (M.empty, 0)
  where 
    step :: (M.Map S.Identifier Symbol, Int) -> S.Param -> (M.Map S.Identifier Symbol, Int)
    step (m, n) (S.Param (typ, i)) = 
      (M.insert i (Param n typ) m, n + 1)

translateFunc :: M.Map S.Identifier Symbol ->  S.DeclAug a -> Func
translateFunc globals funcDef@(S.DefFn _ ps _ _, _) = 
  let initState = Translator (Func []) 0 (M.union globals (paramMap ps))
   in undefined
translateFunc _ _ = error "expected a function declaration"

collectGlobals :: [S.DeclAug a] -> M.Map S.Identifier Symbol
collectGlobals = fst . foldl step (M.empty, 0)
 where
  step ::
    (M.Map S.Identifier Symbol, Int) ->
    S.DeclAug a ->
    (M.Map S.Identifier Symbol, Int)
  step (mapping, counter) (S.Let typ sym _, _) =
    (M.insert sym (Global counter typ) mapping, counter + 1)
  step st _ = st
