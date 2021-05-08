module LiveIntervals (liveIntervals) where

import BytecodeGen (Inst (..))

type LiveInterval = (Integer, Integer)

liveIntervals :: [Inst] -> [LiveInterval]
liveIntervals = undefined
