module Syntax where

type Identifier = String
type Type       = String
type Param      = (Type, Identifier)

data Expr 
  = Let Type Identifier Expr
  | Subs Identifier
  | DefFn Identifier [Param] Type Expr
  | Call Identifier [Expr]
  | If Expr Expr
  | ElseIf Expr Expr
  | Else Expr
  | Semicolon
  | Block [Expr]
  | BinOp Op Expr Expr
  deriving (Eq, Show)

data Op
    = Plus
    | Mul
    | Minus
    | Divide
    | IntDivide
    deriving (Eq, Show)