module Ir.Tac.Translation where

import Ir.Tac.Types
import qualified Parsing.Syntax as S

import qualified Data.Map.Strict as M

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

translateFunc :: M.Map S.Identifier Symbol ->  S.DeclAug a -> Func
translateFunc _ (S.DefFn{}, _) = undefined
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

-- The state variables used in the translation of -- a function from AST -> TAC
-- This is local to each function
data Translation = Translation
  { getFunc :: Func
  , getLocalCounter :: Int
  , getLabelCounter :: Int
  }
