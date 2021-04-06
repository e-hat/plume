module ShowBytecode where

import BytecodeGen
import qualified Data.Map.Strict as M
import Data.Tuple
import Text.Printf (errorShortFormat, printf)

instance Show BytecodeProgram where
  show (BytecodeProgram is tbl) =
    let is' = map ((++ "\n") . show) is
        ftbl = M.fromList $ map swap (M.toList tbl)
        fmtInst :: Integer -> String -> String
        fmtInst ln i' = printf "%08d@\t%s" ln i'
        zipNums :: Integer -> String -> String
        zipNums ln i' = 
          case M.lookup ln ftbl of 
            Just l -> printf "%s:\n%s" l (fmtInst ln i')
            Nothing -> fmtInst ln i'     
     in concat $ zipWith zipNums [1..] is'

instance Show Value where
  show (Register n) = "$" ++ show n
  show (VInt v) = show v
  show (VBool b) = show b
  show (VByte b) = show b
  show (VFloat f) = show f

instance Show Inst where
  show (Ret v) = printf "Ret %s" (show v)
  show (Move v r) = printf "Mov %s -> %s" (show r) (show v)
