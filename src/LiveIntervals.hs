module LiveIntervals (liveIntervals) where

import Bytecode 
import qualified Data.Map.Strict as M

type LiveInterval = (Integer, Integer)

-- live intervals are local to the function
-- this will return a set of live intervals for each function in the program
liveIntervals :: BytecodeProgram -> M.Map Label (M.Map Integer LiveInterval)
liveIntervals b = M.map funcLiveIntervals $ funcs b

funcLiveIntervals :: [Inst] -> M.Map Integer LiveInterval
funcLiveIntervals is = foldl updateIntervalsInst M.empty (zip [0..toInteger $ length is] is)
  where 
    -- takes an instruction and updates the live intervals accordingly
    updateIntervalsInst :: M.Map Integer LiveInterval -> (Integer, Inst) -> M.Map Integer LiveInterval
    updateIntervalsInst m (loc, Move v1 _) = 
      updateIntervalsVal loc m v1
    updateIntervalsInst m (loc, Add v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Sub v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Mul v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Div v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Neg v) = 
      updateIntervalsVal loc m v
    updateIntervalsInst m (loc, IAnd v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, IOr v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Inv v) = 
      updateIntervalsVal loc m v 
    updateIntervalsInst m (loc, Cmp v1 v2) = 
      foldl (updateIntervalsVal loc) m [v1, v2]
    updateIntervalsInst m (loc, Push v) = 
      updateIntervalsVal loc m v 
    updateIntervalsInst m _ = m
    -- updates the intervals according to the value
    updateIntervalsVal :: Integer -> M.Map Integer LiveInterval -> Value -> M.Map Integer LiveInterval 
    updateIntervalsVal loc m (Register r) = updated
      where 
        updateLiveInterval :: Integer -> LiveInterval -> LiveInterval 
        updateLiveInterval l (pMin, pMax) = (min pMin l, max pMax l)
        updated :: M.Map Integer LiveInterval
        updated = M.insert r 
          (updateLiveInterval loc $ M.findWithDefault (loc + 1, loc - 1) r m) m
    updateIntervalsVal _ m _ = m
