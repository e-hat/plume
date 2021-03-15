module Semantics where

import qualified Data.Map.Strict as Map
import Text.Printf (errorShortFormat, printf)

import Syntax

data Symbol = Var Identifier | Func Identifier [Type] deriving (Eq, Ord, Show)

type SymTable = Map.Map Symbol Type

semanticErr :: Node t -> String -> a
semanticErr (Node s c) msg = error $ printf "%s: %s" (show s) msg

getDeclSymbol :: Decl -> Symbol
getDeclSymbol (Let _ i _) = Var i
getDeclSymbol (DefFn i ps _ _) = Func i (map (fst . getParam) ps)

getDeclType :: Decl -> Type
getDeclType (Let t _ _) = t
getDeclType (DefFn _ _ r _) = r
getDeclType _ = "Void"

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

