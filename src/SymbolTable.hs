module SymbolTable where 

import Syntax

import qualified Data.Map.Strict as Map
import Data.Maybe

import Text.Show.Pretty

data TEntry = Single Type | Many [Type] Type deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking and beyond!
type SymTable = Map.Map Identifier TEntry

-- gets the symbol for a let or function definition declaration
getDeclSymbol :: Decl -> Identifier
getDeclSymbol (Let _ i _) = i
getDeclSymbol (DefFn i _ _ _) = i
getDeclSymbol _ = error "getDeclSymbol used on non-let or non-deffn" 

-- gets the type for a let or function definition declaration
getDeclEntry :: Decl -> TEntry
getDeclEntry (Let t _ _) = Single t
getDeclEntry (DefFn _ ps r _) = Many (map (fst . getParam) ps) r
getDeclEntry _ = error "getDeclEntry used on non-let or non-deffn" 

getSymKV :: Decl -> (Identifier, TEntry)
getSymKV (Let t i _ ) = (i, Single t)
getSymKV d@(DefFn i _ _ _) = (i, getDeclEntry d)
getSymKV _ = error "getSymKV used on non-let or non-deffn"

getParamKV :: Param -> (Identifier, TEntry)
getParamKV (Param ti) = (snd ti, Single $ fst ti)

insertParam :: Param -> SymTable -> SymTable
insertParam p = uncurry Map.insert (getParamKV p)

insertDecl :: Decl -> SymTable -> SymTable
insertDecl d = uncurry Map.insert (getSymKV d)

data SymTableTree = SymTableTree
  { getParent :: Maybe SymTableTree,
    getTable :: SymTable,
    getChildren :: [SymTableTree]
  } deriving Show -- TODO: make a Show.Pretty instance for this type

lookupStt :: Identifier -> SymTableTree -> Maybe TEntry
lookupStt sym (SymTableTree p t _) = case Map.lookup sym t of
                                    Nothing -> case p of 
                                                 Nothing -> Nothing 
                                                 Just stt -> lookupStt sym stt
                                    Just t -> Just t


instance PrettyVal SymTableTree where
  prettyVal (SymTableTree mp tbl cs) = 
    Con "TableTree" [Con "Parent" [prettyVal $ isJust mp], Con "Table" [String $ show tbl], Con "Children" (map prettyVal cs)]

