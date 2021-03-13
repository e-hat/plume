module Syntax where

import qualified Text.Parsec as P

type Program = [DeclNode]

data DeclNode = DeclNode
                { getDeclSpan :: SpanRec
                , getDecl     :: Decl
                }
  deriving Show

data ExprNode = ExprNode
                { getExprSpan :: SpanRec
                , getExpr     :: Expr
                }
    
  deriving (Show)

type Identifier = String

type Type = String

type Param = (Type, Identifier)

data Decl 
  = Let Type Identifier ExprNode
  | Reassign Identifier ExprNode
  | DefFn Identifier [Param] Type ExprNode
  | CallDecl Identifier [ExprNode]
  | IfDecl ExprNode DeclNode
  | ElseIfDecl ExprNode DeclNode
  | ElseDecl DeclNode
  | BlockDecl [DeclNode]
  deriving Show

data Expr
  = Subs Identifier
  | CallExpr Identifier [ExprNode]
  | IfExpr ExprNode ExprNode
  | ElseIfExpr ExprNode ExprNode
  | ElseExpr ExprNode
  | BlockExpr [DeclNode] ExprNode
  | BinOp Op ExprNode ExprNode
  | UnaryOp Op ExprNode
  | LitInt Integer
  | LitFloat Double
  | LitString String
  | LitBool Bool
  | LitChar Char
  deriving (Show)

data Op
  = Plus
  | Mul
  | Minus
  | Divide
  | IntDivide
  | Neg
  | And
  | Or
  | Not
  | Less
  | Leq
  | Greater
  | Geq
  | Equal
  | NotEqual
  deriving Show
 
data SpanRec =
  SpanRec
    { getFileName :: String
    , getMinLine :: Int
    , getMaxLine :: Int
    , getMinCol :: Int
    , getMaxCol :: Int
    }
  deriving (Show)

getLineRange :: SpanRec -> (Int, Int)
getLineRange (SpanRec _ a b _ _) = (a, b)

getColRange :: SpanRec -> (Int, Int)
getColRange (SpanRec _ _ _ a b) = (a, b)

spanBtwnSP :: P.SourcePos -> P.SourcePos -> SpanRec
spanBtwnSP s1 s2 =
  SpanRec
    (P.sourceName s1)
    (P.sourceLine s1)
    (P.sourceLine s2)
    (P.sourceColumn s1)
    (P.sourceColumn s2)

instance Semigroup SpanRec where
  (<>) (SpanRec aFname aMinLine aMaxLine aMinCol aMaxCol) (SpanRec bFname bMinLine bMaxLine bMinCol bMaxCol) =
    SpanRec
      aFname
      (min aMinLine bMinLine)
      (max aMaxLine bMaxLine)
      (min aMinCol bMinCol)
      (max aMaxCol bMaxCol)
