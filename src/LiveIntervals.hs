module LiveIntervals (liveIntervals) where

import BytecodeGen (Inst (..), BytecodeProgram (..), Label (..))
import qualified Data.Map.Strict as M

type LiveInterval = (Integer, Integer)

-- live intervals are local to the function
-- this will return live intervals for each function in the program
liveIntervals :: BytecodeProgram -> M.Map Label [LiveInterval]
liveIntervals = undefined

getFuncLines :: BytecodeProgram -> [Integer]
getFuncLines bp = M.elems $ M.filterWithKey isFuncLbl lt 
  where 
    lt = getLabelTable bp
    isFuncLbl :: Label -> Integer -> Bool
    isFuncLbl (FuncLabel _) _ = True 
    isFuncLbl _ _ = False
