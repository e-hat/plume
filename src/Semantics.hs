module Semantics where

import qualified Data.Map.Strict as Map
import SemanticError
import SymbolTable
import Syntax

-- generates a global list of symbols
-- I do NOT want to have forward declaration be a thing
-- reverses input list so that error message shows up for the first
-- occurence of an overlap
-- this function returns a list of *unique* symbols from the global scope
genGlobalSyms :: Program -> SymTable
genGlobalSyms = genGlobalSymsImpl . reverse . getProgram
  where
    givesNewSym :: Decl -> Bool
    givesNewSym Let {} = True
    givesNewSym DefFn {} = True
    givesNewSym _ = False
    genGlobalSymsImpl :: [DeclNode] -> SymTable
    genGlobalSymsImpl [d] =
      if givesNewSym $ getContent d
        then Map.singleton (getDeclSymbol $ getContent d) (getDeclType $ getContent d)
        else Map.empty
    genGlobalSymsImpl (d : ds) =
      let rest = genGlobalSymsImpl ds
          sym = getDeclSymbol $ getContent d
       in if givesNewSym $ getContent d
            then case Map.lookup sym rest of
              Nothing -> Map.insert sym (getDeclType $ getContent d) rest
              Just _ -> semanticErr d "cannot have overlapping symbol declarations in global scope"
            else rest

class Scannable a where
  scan :: a -> SymTableTree -> Either String SymTableTree

scanNode :: (ErrRep t, Scannable t) => SymTableTree -> Node t -> SymTableTree
scanNode base n@(Node sr c) = case scan c base of
  Left msg -> semanticErr n msg
  Right result -> result

validateProgram :: Program -> SymTableTree
validateProgram p =
  let baseTree = SymTableTree Nothing (genGlobalSyms p) []
   in foldl scanNode baseTree (getProgram p)

instance Scannable Decl where
  -- scan handles building the symbol table and checking for scoping issues
  scan l@(Let _ i expr) ct@(SymTableTree pTbl tbl cs) =
    if getDeclSymbol l `Map.member` tbl
      then Left $ "the variable " ++ i ++ " has been created twice in this scope"
      else Right $ SymTableTree pTbl (insertDecl l tbl) (cs ++ [scanNode childTree expr])
    where
      childTree = SymTableTree (Just ct) Map.empty []
  scan r@(Reassign i expr) ct@(SymTableTree pTbl tbl cs) =
    case lookupStt (Var i) ct of
      Nothing -> Left $ "the symbol " ++ i ++ " has not yet been declared in a Let declaration"
      Just _ ->
        Right $ SymTableTree pTbl tbl (cs ++ [scanNode childTree expr])
        where
          childTree = SymTableTree (Just ct) Map.empty []
  scan _ _ = undefined

instance Scannable Expr where
  -- literals, by definition, don't have anything to do with the symbol table
  scan (LitInt _) = Right
