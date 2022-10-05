module Parsing.Syntax where

import qualified Text.Parsec as P
import Text.Printf (printf)
import Text.Show.Pretty

newtype Program = Program {getProgram :: [ASTDeclAug]}

newtype ASTDeclAug = ASTDeclAug {getASTDeclAug :: DeclAug SpanRec}

newtype ASTExprAug = ASTExprAug {getASTExprAug :: ExprAug SpanRec}

type Identifier = String

type Type = String

newtype Param = Param {getParam :: (Type, Identifier)} deriving (Eq)

type DeclAug t = (Decl t, t)

type ExprAug t = (Expr t, t)

voidSentinel :: String 
voidSentinel = "Void"

-- each stmt in Plume is either a declaration or an expression
-- this is an "augmented" tree structure
-- it allows me to maintain the same "shape" of the tree, but carry different
-- data on each node depending on what I'm doing, e.g. I need just span info for
-- all errors, but I'll need a symbol table on each node for type checking.
-- t represents that additional piece of data unrelated to tree structure
data Decl t
  = Let Type Identifier (ExprAug t)
  | Reassign Identifier (ExprAug t)
  | DefFn Identifier [Param] Type (ExprAug t)
  | CallDecl Identifier [ExprAug t]
  | IfDecl (ExprAug t) (DeclAug t) [(ExprAug t, DeclAug t)] (Maybe (DeclAug t))
  | WhileDecl (ExprAug t) (DeclAug t)
  | BlockDecl [DeclAug t]
  deriving (Functor, Foldable, Traversable)

data Expr t
  = Subs Identifier
  | CallExpr Identifier [ExprAug t]
  | -- note that IfExpr REQUIRES an else branch, otherwise a function could only
    -- sometimes return a value
    IfExpr (ExprAug t) (ExprAug t) [(ExprAug t, ExprAug t)] (ExprAug t)
  | BlockExpr [DeclAug t] (ExprAug t)
  | BinOp Op (ExprAug t) (ExprAug t)
  | UnaryOp Op (ExprAug t)
  | LitInt Integer
  | LitFloat Double
  | LitString String
  | LitBool Bool
  | LitChar Char
  | Return
  deriving (Functor, Foldable, Traversable)

data Op
  = Plus
  | Multiply
  | Minus
  | Divide
  | Negate
  | And
  | Or
  | Not
  | Less
  | Leq
  | Greater
  | Geq
  | Equal
  | NotEqual
  deriving (Show, Eq)

data SpanRec = SpanRec
  { getFileName :: String
  , getMinLine :: Int
  , getMaxLine :: Int
  , getMinCol :: Int
  , getMaxCol :: Int
  }
  deriving (Eq)

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
  (<>) (SpanRec aFname aMinLine aMaxLine aMinCol aMaxCol) (SpanRec _ bMinLine bMaxLine bMinCol bMaxCol) =
    SpanRec
      aFname
      (min aMinLine bMinLine)
      (max aMaxLine bMaxLine)
      (min aMinCol bMinCol)
      (max aMaxCol bMaxCol)

instance Show SpanRec where
  show (SpanRec f mnl mxl mnc mxc) =
    printf "(line: %d->%d, col: %d->%d, %s)" mnl mxl mnc mxc f

instance PrettyVal Program where
  prettyVal (Program ns) = prettyVal ns

instance PrettyVal Param where
  prettyVal (Param p) = prettyVal p

instance PrettyVal ASTDeclAug where
  prettyVal (ASTDeclAug (Let t i e, _)) = Con "Let" [String t, String i, prettyVal $ ASTExprAug e]
  prettyVal (ASTDeclAug (Reassign i e, _)) = Con "Reassign" [String i, prettyVal $ ASTExprAug e]
  prettyVal (ASTDeclAug (DefFn i ps t e, _)) =
    Con "DefFn" [Con "FName" [String i], Con "Params" (map prettyVal ps), Con "Return type" [String t], Con "Body" [prettyVal $ ASTExprAug e]]
  prettyVal (ASTDeclAug (CallDecl i es, _)) = Con "CallDecl" [String i, Con "Params passed" [prettyVal $ map ASTExprAug es]]
  prettyVal (ASTDeclAug (IfDecl e d eds md, _)) =
    Con "IfDecl" [Con "Condition" [prettyVal $ ASTExprAug e], Con "IfResult" [prettyVal $ ASTDeclAug d], Con "ElseIfs" (map (prettyVal . augEFPair) eds), Con "Else" [prettyVal (ASTDeclAug <$> md)]]
   where
    augEFPair (e', d') = (ASTExprAug e', ASTDeclAug d')
  prettyVal (ASTDeclAug (BlockDecl ds, _)) = Con "BlockDecl" [prettyVal (map ASTDeclAug ds)]
  prettyVal (ASTDeclAug (WhileDecl cond body, _)) = Con "WhileDecl" [prettyVal $ ASTExprAug cond, prettyVal $ ASTDeclAug body]

instance PrettyVal ASTExprAug where
  prettyVal (ASTExprAug (Subs i, _)) = Con "Subs" [String i]
  prettyVal (ASTExprAug (CallExpr i es, _)) = Con "CallExpr" [String i, Con "Params passed" [prettyVal (map ASTExprAug es)]]
  prettyVal (ASTExprAug (IfExpr c e ees me, _)) =
    Con "IfExpr" [Con "Condition" [prettyVal $ ASTExprAug c], Con "IfResult" [prettyVal $ ASTExprAug e], Con "ElseIfs" (map (prettyVal . augEFPair) ees), Con "Else" [prettyVal (ASTExprAug me)]]
   where
    augEFPair (e1, e2) = (ASTExprAug e1, ASTExprAug e2)
  prettyVal (ASTExprAug (BlockExpr ds e, _)) = Con "BlockExpr" [prettyVal (map ASTDeclAug ds), prettyVal $ ASTExprAug e]
  prettyVal (ASTExprAug (BinOp o a b, _)) = Con "BinOp" [String $ show o, prettyVal $ ASTExprAug a, prettyVal $ ASTExprAug b]
  prettyVal (ASTExprAug (UnaryOp o a, _)) = Con "UnaryOp" [String $ show o, prettyVal $ ASTExprAug a]
  prettyVal (ASTExprAug (LitInt i, _)) = Integer (show i)
  prettyVal (ASTExprAug (LitFloat d, _)) = Float (show d)
  prettyVal (ASTExprAug (LitString s, _)) = String s
  prettyVal (ASTExprAug (LitBool b, _)) = String (show b)
  prettyVal (ASTExprAug (LitChar c, _)) = Char (show c)
  prettyVal (ASTExprAug (Return, _)) = Con "Return" []
