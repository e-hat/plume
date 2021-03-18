module Semantics where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import SemanticError
import SymbolTable
import Syntax

-- generates a global list of symbols
-- I do NOT want to have forward declaration be a thing
-- reverses input list so that error message shows up for the first
-- occurence of an overlap
-- this function returns a list of *unique* symbols from the global scope (either Let's or DefFn's)
genGlobalSyms :: Program -> SymTable
genGlobalSyms = genGlobalSymsImpl . reverse . getProgram
  where
    genGlobalSymsImpl :: [DeclNode] -> SymTable
    genGlobalSymsImpl [d] =
      Map.singleton (getDeclSymbol $ getContent d) (getDeclEntry $ getContent d)
    genGlobalSymsImpl (d : ds) =
      let rest = genGlobalSymsImpl ds
          sym = getDeclSymbol $ getContent d
       in case Map.lookup sym rest of
            Nothing -> Map.insert sym (getDeclEntry $ getContent d) rest
            Just _ -> semanticErr d "cannot have overlapping symbol declarations in global scope"

class Scannable a where
  scan :: a -> SymTableTree -> Either String SymTableTree

scanNode :: (ErrRep t, Scannable t) => SymTableTree -> Node t -> SymTableTree
scanNode base n@(Node sr c) = case scan c base of
  Left msg -> semanticErr n msg
  Right result -> result

validateProgram :: Program -> SymTableTree
validateProgram p =
  let baseTree = SymTableTree Nothing (genGlobalSyms p) []
      handleGlobalNode :: SymTableTree -> DeclNode -> SymTableTree
      -- global let declarations should not add their own name to the scope
      -- as this has already been done in genGlobalSyms, so just process their children
      handleGlobalNode s@(SymTableTree Nothing tb cs) (Node _ (Let _ _ e)) =
        let childTree = SymTableTree (Just s) Map.empty []
         in SymTableTree Nothing tb (cs ++ [scanNode childTree e])
      -- otherwise it is a DeclNode _ DefFn, which can only happen in global scope
      -- and does not add it's symbol to its parent scope
      handleGlobalNode s n = scanNode s n
   in foldl handleGlobalNode baseTree (getProgram p)

instance Scannable Decl where
  --------------------------------------------------------------------------
  -- scan handles building the symbol table tree and checking for scoping issues
  --------------------------------------------------------------------------
  -- a let decl either throws an error renaming a variable in the same scope
  -- or it adds a symbol to the current scope and (plume specific) adds a child scope
  scan l@(Let _ i expr) ct@(SymTableTree pTbl tbl cs) =
    if getDeclSymbol l `Map.member` tbl
      then Left $ "the variable " ++ i ++ " has been created twice in this scope"
      else Right $ SymTableTree pTbl (insertDecl l tbl) (cs ++ [scanNode childTree expr])
    where
      childTree = SymTableTree (Just ct) Map.empty []
  -- a reassignment either references an unknown variable name (could be in parent scopes)
  -- or (plume specific) adds a child scope
  scan (Reassign i expr) ct@(SymTableTree pTbl tbl cs) =
    case lookupStt i ct of
      Nothing -> Left $ "the variable " ++ i ++ " has not yet been declared in a Let declaration"
      Just _ ->
        Right $ SymTableTree pTbl tbl (cs ++ [scanNode childTree expr])
        where
          childTree = SymTableTree (Just ct) Map.empty []
  -- a DefFn is only in global scope, where name collisions have already been handled
  -- so only error if duplicate param name (is this too coupled?).
  -- Then, it starts a child scope that gets initialized to have the
  -- parameter symbols already present
  scan (DefFn i ps r expr) ct@(SymTableTree pTbl tbl cs) =
    -- checks that list of unique param names is not shorter than list of params
    if length (nub (map (snd . getParam) ps)) /= length ps
      then Left $ "duplicate param name in definition of " ++ i
      else
        let innerTbl = foldr insertParam Map.empty ps
            childTree = SymTableTree (Just ct) innerTbl []
         in Right $ SymTableTree pTbl tbl (cs ++ [scanNode childTree expr])
  -- handles an IfDecl control flow. This involes creating trees for each sub expr/decl
  -- then adding them all as children to the original tree
  scan (IfDecl cond decl1 elseifs melse) ct@(SymTableTree pTbl tbl cs) =
    let condTree = scanNode (SymTableTree (Just ct) Map.empty []) cond
        decl1Tree = scanNode (SymTableTree (Just ct) Map.empty []) decl1
        scanElseIfDecl :: SymTableTree -> (ExprNode, DeclNode) -> SymTableTree
        scanElseIfDecl ct2@(SymTableTree pTbl2 tbl2 cs2) (eiexpr, eidecl) =
          let eiexprTree = scanNode (SymTableTree (Just ct2) Map.empty []) eiexpr
              eideclTree = scanNode (SymTableTree (Just ct2) Map.empty []) eidecl
           in SymTableTree pTbl2 tbl2 (cs2 ++ [eiexprTree, eideclTree])
        elseifTrees = map (scanElseIfDecl (SymTableTree (Just ct) Map.empty [])) elseifs
        melseTree = scanNode (SymTableTree (Just ct) Map.empty []) <$> melse
     in -- and finally, append all of the trees as children of the original
        Right $ SymTableTree pTbl tbl (cs ++ [condTree, decl1Tree] ++ elseifTrees ++ maybeToList melseTree)

instance Scannable Expr where
  -- literals are easy as, by definition, they don't have anything to do with the symbol table
  scan LitInt {} ct = Right ct
  scan LitChar {} ct = Right ct
  scan LitString {} ct = Right ct
  scan LitFloat {} ct = Right ct
  scan LitBool {} ct = Right ct
  -- return is kind of a literal for Void, right?
  scan Return ct = Right ct
  -- this is straightforward
  scan (Subs i) ct =
    case lookupStt i ct of
      Nothing -> Left $ "symbol " ++ i ++ " has not yet been declared"
      Just _ -> Right ct
  scan (CallExpr i es) ct@(SymTableTree pTbl tbl cs) = 
    case lookupStt i ct of
      Nothing -> Left $ "attempting to call function " ++ i ++ " that does not exist"
      Just (Single _) -> Left $ i ++ " is a variable but is being called like a function"
      Just (Many ps _) -> 
        if length ps /= length es 
           then Left $ "wrong number of parameters passed to function " ++ i 
           else Right $ SymTableTree pTbl tbl (cs ++ map (scanNode childTree) es) 
             where
               childTree = SymTableTree (Just ct) Map.empty []
