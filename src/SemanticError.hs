module SemanticError 
  ( semanticErr ) where

import Text.Printf (errorShortFormat, printf)

import Syntax

-- general error handling --> this will be improved upon
semanticErr :: (ErrRep t) => Node t -> String -> a
semanticErr (Node s d) = wrapStmtName s (errRep d)

wrapStmtName :: SpanRec -> String -> String -> a
wrapStmtName sr name msg = 
  error $ printf "Error: %s\n In `%s` at %s" msg name (show sr)

class ErrRep a where
  errRep :: a -> String

instance ErrRep Decl where
  errRep (Let _ n _) = "Let Declaration for" ++ n
  errRep (Reassign n _) = "Reassign Declaration for" ++ n
  errRep (DefFn n _ _ _) = "Function Definition Declaration for " ++ n
  errRep (CallDecl n _) = "Function Call Declaration of function" ++ n
  errRep IfDecl {} = "If Statement Declaration"
  errRep BlockDecl {} = "Block Statement Declaration"

instance ErrRep Expr where
  errRep (Subs n) = "Substitution Expression of " ++ n
  errRep (CallExpr n _) = "Function Call Expression" ++ n
  errRep IfExpr {} = "If Statement Expression"
  errRep BlockExpr {} = "Block Statement Expression"
  errRep BinOp {} = "Binary Operation Expression"
  errRep UnaryOp {} = "Unary Operation Expression"
  errRep LitInt {} = "Integer Literal Expression"
  errRep LitString {} = "String Literal Expression"
  errRep LitBool {} = "Boolean Literal Expression"
  errRep LitChar {} = "Character Literal Expression"
