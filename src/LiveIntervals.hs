module LiveIntervals (liveIntervals) where

import BytecodeGen (Inst (..), BytecodeProgram (..), Label (..))
import qualified Data.Map.Strict as M

type LiveInterval = (Integer, Integer)

liveIntervals :: [Inst] -> [LiveInterval]
liveIntervals = undefined

getFuncLines :: BytecodeProgram -> [Integer]
getFuncLines bp = M.elems $ M.filterWithKey isFuncLbl lt 
  where 
    lt = getLabelTable bp
    isFuncLbl :: Label -> Integer -> Bool
    isFuncLbl (FuncLabel _) _ = True 
    isFuncLbl _ _ = False
    
