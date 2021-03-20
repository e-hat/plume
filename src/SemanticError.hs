module SemanticError (astSemanticErr, ErrRep) where

import Syntax
import Text.Printf (errorShortFormat, printf)

-- general error handling
astSemanticErr :: (ErrRep t) => (t, SpanRec) -> String -> a
astSemanticErr (d, s)  = wrapStmtName s (errRep d)
  where
    wrapStmtName :: SpanRec -> String -> String -> a
    wrapStmtName sr name msg =
      error $ printf "\n****************\nError: %s\n In `%s` at %s\n****************" msg name (show sr)

class ErrRep a where
  errRep :: a -> String

instance ErrRep (Decl t) where
  errRep (Let _ n _) = "Let Declaration for " ++ n
  errRep (Reassign n _) = "Reassign Declaration for " ++ n
  errRep (DefFn n _ _ _) = "Function Definition Declaration for " ++ n
  errRep (CallDecl n _) = "Function Call Declaration of function " ++ n
  errRep IfDecl {} = "If Statement Declaration"
  errRep BlockDecl {} = "Block Statement Declaration"

instance ErrRep (Expr t) where
  errRep (Subs n) = "Substitution Expression of " ++ n
  errRep (CallExpr n _) = "Function Call Expression of " ++ n
  errRep IfExpr {} = "If Statement Expression"
  errRep BlockExpr {} = "Block Statement Expression"
  errRep BinOp {} = "Binary Operation Expression"
  errRep UnaryOp {} = "Unary Operation Expression"
  errRep LitInt {} = "Integer Literal Expression"
  errRep LitString {} = "String Literal Expression"
  errRep LitBool {} = "Boolean Literal Expression"
  errRep LitChar {} = "Character Literal Expression"
