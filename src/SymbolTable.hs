module SymbolTable where 

import Syntax
import qualified Data.Map.Strict as Map

-- the only valid symbols in Plume, variables and functions
-- note that functions can be overloaded in this language
data Symbol = Var Identifier | Func Identifier [Type] deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking and beyond!
type SymTable = Map.Map Symbol Type

-- gets the symbol for a let or function definition declaration
getDeclSymbol :: Decl -> Symbol
getDeclSymbol (Let _ i _) = Var i
getDeclSymbol (DefFn i ps _ _) = Func i (map (fst . getParam) ps)
getDeclSymbol _ = error "getDeclSymbol used on non-let or non-deffn" 

-- gets the type for a let or function definition declaration
getDeclType :: Decl -> Type
getDeclType (Let t _ _) = t
getDeclType (DefFn _ _ r _) = r
getDeclType _ = error "getDeclType used on non-let or non-deffn" 

getSymKV :: Decl -> (Symbol, Type)
getSymKV (Let t i _ ) = (Var i, t)
getSymKV (DefFn i ps r _) = (Func i (map (fst . getParam) ps), r)
getSymKV _ = undefined

insertDecl :: Decl -> SymTable -> SymTable
insertDecl d = uncurry Map.insert (getSymKV d)

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



