module Semantics.Error (astSemanticErr, typeError, ErrRep) where

import Parsing.Syntax
import Semantics.SymbolTable
import Text.Printf (printf)

-- general error handling
astSemanticErr :: (ErrRep t) => (t, SpanRec) -> String -> String
astSemanticErr (d, s) =
    wrapStmtName s (errRep d)
  where
    wrapStmtName :: SpanRec -> String -> String -> String
    wrapStmtName sr name msg =
        printf "%s\nError: %s\n In `%s` at %s\n%s" starBlock msg name (show sr) starBlock

typeError :: (ErrRep t, ErrRep u) => (t, SymData) -> Type -> (u, SymData) -> Type -> String
typeError (n1, SymData _ s1) t1 (n2, SymData _ s2) =
    wrapStmtName (errRep n1) s1 t1 (errRep n2) s2
  where
    wrapStmtName :: String -> SpanRec -> Type -> String -> SpanRec -> Type -> String
    wrapStmtName sym1 sr1 ty1 sym2 sr2 ty2 =
        printf "%s\nError: could not match type `%s` with type `%s`\nfor symbols `%s` at %s and `%s` at %s respectively\n%s" starBlock ty1 ty2 sym1 (show sr1) sym2 (show sr2) starBlock

starBlock :: String
starBlock = "*******************************************"

class ErrRep a where
    errRep :: a -> String

instance ErrRep (Decl t) where
    errRep (Let _ n _) = "Let Declaration for " ++ n
    errRep (Reassign n _) = "Reassign Declaration for " ++ n
    errRep (DefFn n _ _ _) = "Function Definition Declaration for " ++ n
    errRep (CallDecl n _) = "Function Call Declaration of function " ++ n
    errRep IfDecl{} = "If Statement Declaration"
    errRep BlockDecl{} = "Block Statement Declaration"

instance ErrRep (Expr t) where
    errRep (Subs n) = "Substitution Expression of " ++ n
    errRep (CallExpr n _) = "Function Call Expression of " ++ n
    errRep IfExpr{} = "If Statement Expression"
    errRep BlockExpr{} = "Block Statement Expression"
    errRep BinOp{} = "Binary Operation Expression"
    errRep UnaryOp{} = "Unary Operation Expression"
    errRep LitInt{} = "Integer Literal Expression"
    errRep LitString{} = "String Literal Expression"
    errRep LitBool{} = "Boolean Literal Expression"
    errRep LitChar{} = "Character Literal Expression"
    errRep LitFloat{} = "Float Literal Expression"
    errRep Return = "Return Expression"
