module Semantics where

import qualified Data.Map.Strict as Map

import Syntax
import SemanticError

-- the only valid symbols in Plume, variables and functions
-- note that functions can be overloaded in this language
data Symbol = Var Identifier | Func Identifier [Type] deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking
type SymTable = Map.Map Symbol Type

-- gets the symbol for a let or function definition declaration
getDeclSymbol :: Decl -> Symbol
getDeclSymbol (Let _ i _) = Var i
getDeclSymbol (DefFn i ps _ _) = Func i (map (fst . getParam) ps)

-- gets the type for a let or function definition declaration
getDeclType :: Decl -> Type
getDeclType (Let t _ _) = t
getDeclType (DefFn _ _ r _) = r
getDeclType _ = "Void"

-- generates a global list of symbols
-- I do NOT want to have forward declaration be a thing
genGlobalSyms :: [DeclNode] -> SymTable
genGlobalSyms [d] =  
  Map.singleton (getDeclSymbol $ getContent d) (getDeclType $ getContent d)
genGlobalSyms (d:ds) = 
  let 
    rest = genGlobalSyms ds
    sym  = getDeclSymbol $ getContent d
  in
    case Map.lookup sym rest of
      Nothing -> Map.insert sym (getDeclType $ getContent d) rest
      Just _  -> semanticErr d "symbols cannot share name in global scope"

