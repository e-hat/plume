module ShowBytecode where

import BytecodeGen
import qualified Data.Map.Strict as M
import Data.Tuple
import Text.Printf (errorShortFormat, printf)

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
  show (Register n) = "$" ++ show n
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
