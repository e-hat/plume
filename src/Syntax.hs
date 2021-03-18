module Syntax where

import Data.List (intercalate)
import qualified Text.Parsec as P
import Text.Printf (errorShortFormat, printf)
import Text.Show.Pretty

newtype Program = Program {getProgram :: [DeclNode]}

-- Nodes specialized based on content types
data Node t = Node
  { getSpan :: SpanRec,
    getContent :: t
  } deriving Eq

type DeclNode = Node Decl
type ExprNode = Node Expr

type Identifier = String
type Type = String

newtype Param = Param {getParam :: (Type, Identifier)} deriving Eq

-- each stmt in Plume is either a declaration or an expression
data Decl
  = Let Type Identifier ExprNode
  | Reassign Identifier ExprNode
  | DefFn Identifier [Param] Type ExprNode
  | CallDecl Identifier [ExprNode]
  | IfDecl ExprNode DeclNode [(ExprNode, DeclNode)] (Maybe DeclNode)
  | BlockDecl [DeclNode]
  deriving Eq

data Expr
  = Subs Identifier
  | CallExpr Identifier [ExprNode]
  | IfExpr ExprNode ExprNode [(ExprNode, ExprNode)] (Maybe ExprNode)
  | BlockExpr [DeclNode] ExprNode
  | BinOp Op ExprNode ExprNode
  | UnaryOp Op ExprNode
  | LitInt Integer
  | LitFloat Double
  | LitString String
  | LitBool Bool
  | LitChar Char
  | Return
  deriving Eq

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
  deriving (Show, Eq)

data SpanRec = SpanRec
  { getFileName :: String,
    getMinLine :: Int,
    getMaxLine :: Int,
    getMinCol :: Int,
    getMaxCol :: Int
  } deriving Eq

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

instance Show SpanRec where
  show (SpanRec f mnl mxl mnc mxc) =
    printf "(line: %d->%d, col: %d->%d, %s)" mnl mxl mnc mxc f

instance PrettyVal Program where
  prettyVal (Program ns) = prettyVal ns

instance PrettyVal Param where
  prettyVal (Param p) = prettyVal p

instance (PrettyVal t) => PrettyVal (Node t) where
  prettyVal (Node _ c) = prettyVal c

instance PrettyVal Decl where
  prettyVal (Let t i e) = Con "Let" [String t, String i, prettyVal e]
  prettyVal (Reassign i e) = Con "Reassign" [String i, prettyVal e]
  prettyVal (DefFn i ps t e) =
    Con "DefFn" [Con "FName" [String i], Con "Params" (map prettyVal ps), Con "Return type" [String t], Con "Body" [prettyVal e]]
  prettyVal (CallDecl i es) = Con "CallDecl" [String i, Con "Params passed" [prettyVal es]]
  prettyVal (IfDecl e d eds md) =
    Con "IfDecl" [Con "Condition" [prettyVal e], Con "IfResult" [prettyVal d], Con "ElseIfs" (map prettyVal eds), Con "Else" [prettyVal md]]
  prettyVal (BlockDecl ds) = Con "BlockDecl" [prettyVal ds]

instance PrettyVal Expr where
  prettyVal (Subs i) = Con "Subs" [String i]
  prettyVal (CallExpr i es) = Con "CallExpr" [String i, Con "Params passed" [prettyVal es]]
  prettyVal (IfExpr c e ees me) =
    Con "IfExpr" [Con "Condition" [prettyVal c], Con "IfResult" [prettyVal e], Con "ElseIfs" (map prettyVal ees), Con "Else" [prettyVal me]]
  prettyVal (BlockExpr ds e) = Con "BlockExpr" [prettyVal ds, prettyVal e]
  prettyVal (BinOp o a b) = Con "BinOp" [String $ show o, prettyVal a, prettyVal b]
  prettyVal (UnaryOp o a) = Con "UnaryOp" [String $ show o, prettyVal a]
  prettyVal (LitInt i) = Integer (show i)
  prettyVal (LitFloat d) = Float (show d)
  prettyVal (LitString s) = String s
  prettyVal (LitBool b) = String (show b)
  prettyVal (LitChar c) = Char (show c)
  prettyVal Return = Con "Return" []

