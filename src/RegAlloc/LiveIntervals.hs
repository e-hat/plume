module RegAlloc.LiveIntervals where

import Bytecode.Types

import qualified Data.Map.Strict as M

type LiveInterval = (Integer, Integer)
type VRegIntervals = M.Map Integer LiveInterval

-- live intervals are local to the function
-- this will return a mapping of live intervals for each function in the program
liveIntervals :: [Inst] -> VRegIntervals
liveIntervals is = foldl updateIntervalsInst M.empty (zip [0 .. toInteger $ length is] is)
 where
  -- takes an instruction and updates the mapping accordingly
  updateIntervalsInst :: VRegIntervals -> (Integer, Inst) -> VRegIntervals
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
  -- params are ordered for currying purposes
  updateIntervalsVal :: Integer -> VRegIntervals -> Value -> VRegIntervals
  updateIntervalsVal loc m (VRegister r) = updated
   where
    updateLiveInterval :: Integer -> LiveInterval -> LiveInterval
    updateLiveInterval l (pMin, pMax) = (min pMin l, max pMax l)
    updated :: M.Map Integer LiveInterval
    updated =
      M.insert
        r
        (updateLiveInterval loc $ M.findWithDefault (loc + 1, loc - 1) r m)
        m
  updateIntervalsVal _ m _ = m
