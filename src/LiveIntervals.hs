module LiveIntervals (liveIntervals) where

import BytecodeGen (Inst (..), BytecodeProgram (..), Label (..))
import qualified Data.Map.Strict as M
import Data.List

type LiveInterval = (Integer, Integer)

-- live intervals are local to the function
-- this will return live intervals for each function in the program
liveIntervals :: BytecodeProgram -> M.Map Label [LiveInterval]
liveIntervals = undefined

funcs :: BytecodeProgram -> M.Map Label [Inst]
funcs b = M.fromList $ 
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
sliceLocs (beg:end:as) bs =
  take (end - beg) (drop beg bs) : sliceLocs (end : as) bs
