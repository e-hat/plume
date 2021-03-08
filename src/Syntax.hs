module Syntax where

import qualified Text.Parsec as P

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

data ASTNode =
  ASTNode
    { getSpan :: SpanRec
    , getExpr :: Expr
    }
  deriving (Show)

type Identifier = String

type Type = String

type Param = (Type, Identifier)

-- note that in Plume, EVERYTHING is an expression
-- this will make parsing "if's as expressions" and the like
-- easier, I hope ¯\_(ツ)_/¯
data Expr
  = Let Type Identifier ASTNode
  | Reassign Identifier ASTNode
  | Subs Identifier
  | DefFn Identifier [Param] Type ASTNode
  | Call Identifier [ASTNode]
  | If ASTNode ASTNode
  | ElseIf ASTNode ASTNode
  | Else ASTNode
  | Semicolon
  | Block [ASTNode]
  | BinOp Op ASTNode ASTNode
  | UnaryOp Op ASTNode
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
  deriving (Eq, Show)
