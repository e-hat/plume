module Semantics where

import qualified Data.Map.Strict as Map

import Syntax

data Symbol = Var Identifier | Func Identifier [Type] 

newtype SymTable = SymTable { getTable :: Map.Map Symbol Type } 

getDeclSymbol :: Decl -> (Symbol, Type)
getDeclSymbol (Let t i _) = (Var i, t)
getDeclSymbol (DefFn i ps r _) = (Func i (map (fst . getParam) ps), r)
