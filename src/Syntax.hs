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
  | UnaryOp Op Expr
  | LitInt Integer 
  | LitFloat Double 
  | LitString String
  | LitBool Bool
  | LitChar Char
  deriving (Eq, Show)

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

