module Ir.Tac.Types where

import qualified Parsing.Syntax as S

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

newtype Label = Label {getLabel :: Int}

data Term
  = Subs Symbol
  | LitInt Integer
  | LitFloat Double
  | LitString String
  | LitBool Bool
  | LitChar Char

newtype FuncCall a = FuncCall {getFuncCall :: (String, [a])}
  deriving (Functor, Foldable, Traversable)

-- An expr contains less than 4 terms, unless it is a function call in which it has a list of them
data Expr a
  = Bin a S.Op a
  | Un S.Op a
  | None a
  | FuncCallExpr Type (FuncCall a)
  deriving (Functor, Foldable, Traversable)

-- These types have been formulated such that a maximum of 3 terms are in each
-- instruction
data GeneralInst a
  = Assignment Symbol (Expr a)
  | Cond {getPred :: Expr a, getConsequent :: [GeneralInst a], getAlternative :: [GeneralInst a]}
  | While {getWhileCond :: Expr a, getWhileBody :: [GeneralInst a]}
  | Block [GeneralInst a]
  | Return (Maybe (Expr a))
  | IgnoreReturnValCall (FuncCall a)
  deriving (Functor, Foldable, Traversable)

type Inst = GeneralInst Term

data Func = Func {getParamTypes :: [Type], getReturnType :: Type, getFunc :: [Inst]}

newtype Program = Program {getProgram :: M.Map String Func}

class Typed a where
  getType :: a -> Type

instance Typed Symbol where
  getType (Global _ t) = t
  getType (Local _ t) = t
  getType (Param _ t) = t

instance Typed Term where
  getType (LitInt _) = "Int"
  getType (LitFloat _) = "Float"
  getType (LitString _) = "String"
  getType (LitBool _) = "Bool"
  getType (LitChar _) = "Char"
  getType (Subs sym) = getType sym

instance Typed a => Typed (Expr a) where
  getType (Bin l op _)
    | isRel op = "Bool"
    | otherwise = getType l
  getType (Un _ t) = getType t
  getType (None a) = getType a
  getType (FuncCallExpr t _) = t

-- helps differentiate relationals so that if statements with relationals
-- as the root node of the condition can be generated more efficiently
isRel :: S.Op -> Bool
isRel S.Leq = True
isRel S.Less = True
isRel S.Geq = True
isRel S.Greater = True
isRel S.Equal = True
isRel S.NotEqual = True
isRel _ = False

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
  show (LitFloat f) = show f
  show (LitString s) = show s
  show (LitBool b) = map toLower $ show b
  show (LitChar c) = show c

instance Show a => Show (FuncCall a) where
  show (FuncCall (name, ps)) = printf "%s(%s)" name (intercalate ", " (map show ps))

instance Show a => Show (Expr a) where
  show (Bin l op r) = printf "%s %s %s" (show l) (show (OpWrapper op)) (show r)
  show (Un op t) = printf "%s(%s)" (show (OpWrapper op)) (show t)
  show (None t) = show t
  show (FuncCallExpr _ fcall) = show fcall

newtype OpWrapper = OpWrapper {getOp :: S.Op}
instance Show OpWrapper where
  show (OpWrapper S.Plus) = "+"
  show (OpWrapper S.Minus) = "-"
  show (OpWrapper S.Multiply) = "*"
  show (OpWrapper S.Divide) = "/"
  show (OpWrapper S.Negate) = "-"
  show (OpWrapper S.And) = "&&"
  show (OpWrapper S.Or) = "||"
  show (OpWrapper S.Not) = "!"
  show (OpWrapper S.Less) = "<"
  show (OpWrapper S.Leq) = "<="
  show (OpWrapper S.Greater) = ">"
  show (OpWrapper S.Geq) = ">="
  show (OpWrapper S.Equal) = "=="
  show (OpWrapper S.NotEqual) = "!="

instance Show Func where
  show = intercalate "\n" . concatMap showInst . getFunc

showInst :: Inst -> [String]
showInst (Assignment dst src) = [printf "%s := %s" (show dst) (show src)]
showInst (Cond p c []) = printf "if %s then" (show p) : map ("  " ++) (concatMap showInst c)
showInst (Cond p c e) = showInst (Cond p c []) ++ ["else"] ++ map ("  " ++) (concatMap showInst e)
showInst (While cond body) = printf "while %s:" (show cond) : map ("  " ++) (concatMap showInst body)
showInst (Block b) = map ("  " ++) (concatMap showInst b)
showInst (Return (Just t)) = [printf "return %s" (show t)]
showInst (Return Nothing) = ["return"]
showInst (IgnoreReturnValCall fcall) = [show fcall]

instance Show Program where
  show = intercalate "\n\n" . map step . M.assocs . getProgram
   where
    step :: (String, Func) -> String
    step (fname, f) = printf "define %s\n%s" fname (show f)
