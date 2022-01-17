module Ir.Tac.Types where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Text.Printf (printf)

-- This module defines a Three Address Code (henceforth TAC)
-- that will be used as an intermediate representation for the input program

-- Used for typing terms in TAC
type Type = String

data Symbol = Global Int Type | Local Int Type | Param Int Type

getId :: Symbol -> Int
getId (Global n _) = n
getId (Local n _) = n
getId (Param n _) = n

getSType :: Symbol -> Type
getSType (Global _ t) = t
getSType (Local _ t) = t
getSType (Param _ t) = t

newtype Label = Label {getLabel :: Int}

data Term = Subs Symbol | LitInt Int | LitBool Bool

getType :: Term -> Type
getType (LitInt _) = "Int"
getType (LitBool _) = "Bool"
getType (Subs sym) = getSType sym

-- The TAC supports binary and unary operators
data BinOp = Plus | Sub | Mult
data UnOp = Neg | Not

newtype FuncCall a = FuncCall {getFuncCall :: (String, [a])} 
  deriving (Functor, Foldable, Traversable)

-- An expr contains less than 4 terms, unless it is a function call in which it has a list of them
data Expr a
  = Bin a BinOp a
  | Un UnOp a
  | None a
  | FuncCallExpr (FuncCall a)
  deriving (Functor, Foldable, Traversable)

-- These types have been formulated such that a maximum of 3 terms are in each
-- instruction
data GeneralInst a
  = Assignment Symbol (Expr a)
  | Cond (Expr a) Label
  | Goto Label
  | Return (Maybe Term)
  | IgnoreReturnValCall (FuncCall a)
  deriving (Functor, Foldable, Traversable)

type Inst = GeneralInst Term

newtype Func = Func {getFunc :: [(Maybe Label, Inst)]}

newtype Program = Program {getProgram :: M.Map String Func}

-- Show instances for the types defined above
instance Show Symbol where
  show l@Local{} = printf "$%d" (getId l)
  show g@Global{} = printf "g%d" (getId g)
  show p@Param{} = printf "p%d" (getId p)

instance Show Label where
  show = printf "*%06d" . getLabel

instance Show Term where
  show (Subs l) = show l
  show (LitInt i) = show i
  show (LitBool b) = map toLower $ show b

instance Show BinOp where
  show Plus = "+"
  show Sub = "-"
  show Mult = "*"

instance Show UnOp where
  show Neg = "-"
  show Not = "!"

instance Show a => Show (FuncCall a) where
  show (FuncCall (name, ps)) = printf "%s(%s)" name (intercalate ", " (map show ps))

instance Show a => Show (Expr a) where
  show (Bin l op r) = printf "%s %s %s" (show l) (show op) (show r)
  show (Un op t) = printf "%s(%s)" (show op) (show t)
  show (None t) = show t
  show (FuncCallExpr fcall) = show fcall

instance Show a => Show (GeneralInst a) where
  show (Assignment dst src) = printf "%s := %s" (show dst) (show src)
  show (Cond cond dst) = printf "if %s then goto %s" (show cond) (show dst)
  show (Goto dst) = printf "goto %s" (show dst)
  show (Return Nothing) = "return"
  show (Return (Just t)) = printf "return %s" (show t)
  show (IgnoreReturnValCall fcall) = show fcall
