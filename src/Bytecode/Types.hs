module Bytecode.Types where

import Data.List
import qualified Data.Map.Strict as M

import Data.Tuple
import Text.Printf (printf)

-- types of values that can be moved around
data Value
  -- distinction between physical and virtual registers is important for regalloc
  = PRegister Integer
  | VRegister Integer 
  | VBool Bool
  | VInt Integer
  | VFloat Double
  | VByte Char
  | SyscallCode SyscallCode
  deriving (Eq)

data SyscallCode = Exit deriving (Show, Eq)

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
data SyscallSchema = SyscallSchema [Value] SyscallCode

newSyscallSchema :: [Integer] -> SyscallCode -> SyscallSchema
newSyscallSchema prs = SyscallSchema (map PRegister prs)

data BytecodeProgram = BytecodeProgram
  { getInstructions :: [Inst]
  , getLabelTable :: M.Map Label Integer
  }

type BytecodeFuncs = M.Map Label [Inst]

funcs :: BytecodeProgram -> BytecodeFuncs 
funcs b =
  M.fromList $
    zip (map fst flocs) (sliceLocs (map (fromInteger . snd) flocs) is)
 where
  is = getInstructions b
  flbls = funcLbls b
  flocs = sortBy (\(_, x) (_, y) -> compare x y) (M.assocs flbls)

funcLbls :: BytecodeProgram -> M.Map Label Integer
funcLbls b = M.filterWithKey isFuncLbl lt
 where
  lt = getLabelTable b
  isFuncLbl :: Label -> Integer -> Bool
  isFuncLbl (FuncLabel _) _ = True
  isFuncLbl _ _ = False

sliceLocs :: [Int] -> [b] -> [[b]]
sliceLocs [] _ = undefined -- if this happens, the function has been used wrong
sliceLocs [i] bs = [drop i bs]
sliceLocs (beg : end : as) bs =
  take (end - beg) (drop beg bs) : sliceLocs (end : as) bs

-- pre-defined schema section
exitSchema :: SyscallSchema
exitSchema = newSyscallSchema [1] Exit

-- equivalent of %eax in this bytecode is $0
retReg :: Value
retReg = PRegister 0

prettifyLabel :: Integer -> String
prettifyLabel = printf "*%06d*"

-- show instances
instance Show BytecodeProgram where
  show (BytecodeProgram is tbl) =
    let is' = map ((++ "\n") . show) is
        ftbl = map swap (M.toList tbl)
        fmtInst :: Integer -> String -> String
        fmtInst ln i' = printf "%08d@\t%s" ln i'
        zipNums :: Integer -> String -> String
        zipNums ln i' =
          case map snd (filter ((== ln) . fst) ftbl) of
            l : ls ->
              printf "%s:\n" (show l)
                ++ concatMap (printf "%s:\n" . show) ls
                ++ fmtInst ln i'
            [] -> fmtInst ln i'
     in concat $ zipWith zipNums [0 ..] is'

instance Show Label where
  show (JmpLabel l) = l
  show (FuncLabel l) = l

instance Show Value where
  show (VRegister n) = "v" ++ show n
  show (PRegister n) = "p" ++ show n
  show (VInt v) = show v
  show (VBool b) = show b
  show (VByte b) = show b
  show (VFloat f) = show f
  show (SyscallCode c) = show c

instance Show Inst where
  show (Move v r) = printf "Mov %s -> %s" (show v) (show r)
  show Ret = "Ret"
  show (Add v1 r) = printf "Add (%s, %s)" (show v1) (show r)
  show (Sub v1 r) = printf "Sub (%s, %s)" (show v1) (show r)
  show (Mul v1 r) = printf "Mul (%s, %s)" (show v1) (show r)
  show (Div v1 r) = printf "Div (%s, %s)" (show v1) (show r)
  show (IAnd v1 r) = printf "And (%s, %s)" (show v1) (show r)
  show (IOr v1 r) = printf "Or (%s, %s)" (show v1) (show r)
  show (Neg r) = printf "Neg %s" (show r)
  show (Inv r) = printf "Inv %s" (show r)
  show (Cmp v1 v2) = printf "Cmp %s, %s" (show v1) (show v2)
  show (Jmp l) = printf "Jmp %s" l
  show (JmpEqual l) = printf "JmpEqual %s" l
  show (JmpNotEqual l) = printf "JmpNotEqual %s" l
  show (JmpL l) = printf "JmpL %s" l
  show (JmpLeq l) = printf "JmpLeq %s" l
  show (JmpG l) = printf "JmpG %s" l
  show (JmpGeq l) = printf "JmpGeq %s" l
  show (Push v) = printf "Push %s" (show v)
  show (Pop v) = printf "Pop %s" (show v)
  show (Call l) = printf "Call %s" l
  show Syscall = "Syscall"
