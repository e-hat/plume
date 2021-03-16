module Semantics where

import qualified Data.Map.Strict as Map

import SemanticError
import Syntax

-- the only valid symbols in Plume, variables and functions
-- note that functions can be overloaded in this language
data Symbol = Var Identifier | Func Identifier [Type] deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking and beyond!
type SymTable = Map.Map Symbol Type

-- gets the symbol for a let or function definition declaration
getDeclSymbol :: Decl -> Symbol
getDeclSymbol (Let _ i _) = Var i
getDeclSymbol (DefFn i ps _ _) = Func i (map (fst . getParam) ps)
-- other types of declarations are anonymous
getDeclSymbol _ = undefined

-- gets the type for a let or function definition declaration
getDeclType :: Decl -> Type
getDeclType (Let t _ _) = t
getDeclType (DefFn _ _ r _) = r
getDeclType _ = "Void"

-- generates a global list of symbols
-- I do NOT want to have forward declaration be a thing
-- reverses input list so that error message shows up for the first
-- occurence of an overlap
-- this function returns a list of *unique* symbols from the global scope
genGlobalSyms :: Program -> SymTable
genGlobalSyms = genGlobalSymsImpl . reverse . getProgram
  where
    genGlobalSymsImpl :: [DeclNode] -> SymTable
    genGlobalSymsImpl [d] =
      Map.singleton (getDeclSymbol $ getContent d) (getDeclType $ getContent d)
    genGlobalSymsImpl (d : ds) =
      let rest = genGlobalSymsImpl ds
          sym = getDeclSymbol $ getContent d
       in case Map.lookup sym rest of
            Nothing -> Map.insert sym (getDeclType $ getContent d) rest
            Just _ -> semanticErr d "cannot have overlapping symbol declarations in global scope"

data SymTableTree = SymTableTree
  { getParent :: Maybe SymTableTree,
    getTable :: SymTable,
    getChildren :: [SymTableTree]
  } deriving Show -- TODO: make a Show.Pretty instance for this type

lookupStt :: Symbol -> SymTableTree -> Maybe Type
lookupStt sym (SymTableTree p t _) = case Map.lookup sym t of
                                    Nothing -> case p of 
                                                 Nothing -> Nothing 
                                                 Just stt -> lookupStt sym stt
                                    Just t -> Just t

class Validated a where
  validate :: a -> SymTableTree -> Either String SymTableTree

validateNode :: (ErrRep t, Validated t) => Node t -> SymTableTree -> SymTableTree
validateNode n@(Node sr c) base = case validate c base of
                             Left msg -> semanticErr n msg
                             Right result -> result

instance Validated Decl where
  validate d base = undefined

instance Validated Expr where
  validate e base = undefined

validateProgram :: Program -> SymTableTree
validateProgram p = 
  let baseTree = SymTableTree Nothing (genGlobalSyms p) [] 
  in 
    foldr validateNode baseTree (getProgram p) 
  
