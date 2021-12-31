module Ir.Tac where 

import Text.Printf (printf)
import Data.Char
import qualified Data.Map.Strict as M

-- This module defines a Three Address Code (henceforth TAC) 
-- that will be used as an intermediate representation for the input program

-- Used for typing terms in TAC
data Type = Int | Bool deriving Show

-- Represents local variables, which have an id and a type
newtype Local = Local {getLocal :: (Int, Type)}

getId :: Local -> Int
getId = fst . getLocal

getLocalType :: Local -> Type 
getLocalType = snd . getLocal

newtype Label = Label {getLabel :: Int}

data Term = Subs Local | LitInt Int | LitBool Bool

getType :: Term -> Type 
getType (Subs (Local (_, typ))) = typ
getType (LitInt _) = Int 
getType (LitBool _) = Bool

-- The TAC supports binary and unary operators
data BinOp = Plus | Sub | Mult 
data UnOp = Neg | Not 

data Expr = Bin Term BinOp Term | Un UnOp Term | None Term

-- These types have been formulated such that a maximum of 3 terms are in each 
-- instruction
data Inst = Assignment Local Expr | Cond Expr Label | Goto Label

newtype Func = Func {getFunc :: [(Maybe Label, Inst)]} 

newtype Program = Program { getProgram :: M.Map String Func }

-- Show instances for the types defined above
instance Show Local where 
  show = printf "$%d" . getId

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

instance Show Expr where 
  show (Bin l op r) = printf "%s %s %s" (show l) (show op) (show r)
  show (Un op t) = printf "%s(%s)" (show op) (show t)
  show (None t) = show t

instance Show Inst where 
  show (Assignment dst src) = printf "%s := %s" (show dst) (show src)
  show (Cond cond dst) = printf "if %s then goto %s" (show cond) (show dst)
  show (Goto dst) = printf "goto %s" (show dst)
