module Bytecode where

import Data.List
import qualified Data.Map.Strict as M

import Text.Printf (errorShortFormat, printf)

-- types of values that can be moved around
data Value
  = Register Integer
  | VBool Bool
  | VInt Integer
  | VFloat Double
  | VByte Char
  | SyscallCode SyscallCode

data SyscallCode = Exit deriving (Show)

data Inst
  = Ret
  | Move Value Value
  | Add Value Value
  | Sub Value Value
  | Mul Value Value
  | Div Value Value
  | Neg Value
  | IAnd Value Value
  | IOr Value Value
  | Inv Value
  | Cmp Value Value
  | Jmp String
  | JmpNotEqual String
  | JmpEqual String
  | JmpGeq String
  | JmpLeq String
  | JmpL String
  | JmpG String
  | Push Value
  | Pop Value
  | Call String
  | Syscall

data Label = FuncLabel String | JmpLabel String deriving (Eq, Ord)

-- list of registers that it uses, then SyscallCode
data SyscallSchema = SyscallSchema [Integer] SyscallCode

data BytecodeProgram = BytecodeProgram
  { getInstructions :: [Inst]
  , getLabelTable :: M.Map Label Integer
  }

funcs :: BytecodeProgram -> M.Map Label [Inst]
funcs b =
  M.fromList $
    zip (map fst flocs) (sliceLocs (map (fromInteger . snd) flocs) is)
 where
  is = getInstructions b
  flbls = funcLbls b
  flocs = sortBy (\(_, a) (_, b) -> compare a b) (M.assocs flbls)

funcLbls :: BytecodeProgram -> M.Map Label Integer
funcLbls b = M.filterWithKey isFuncLbl lt
 where
  lt = getLabelTable b
  isFuncLbl :: Label -> Integer -> Bool
  isFuncLbl (FuncLabel _) _ = True
  isFuncLbl _ _ = False

sliceLocs :: [Int] -> [b] -> [[b]]
sliceLocs [i] bs = [drop i bs]
sliceLocs (beg : end : as) bs =
  take (end - beg) (drop beg bs) : sliceLocs (end : as) bs

-- pre-defined schema section
exitSchema = SyscallSchema [1] Exit

-- equivalent of %eax in this bytecode is $0
retReg :: Integer
retReg = 0

prettifyLabel :: Integer -> String
prettifyLabel = printf "*%06d*"
