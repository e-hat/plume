module RegAlloc.LinearScan (linearScanRegalloc) where

import Bytecode.Types
import qualified Data.Map.Strict as M
import RegAlloc.LiveIntervals
import RegAlloc.Shared

import Control.Monad.State
import Data.Bifunctor
import Data.List
import qualified Data.SortedList as SL
import Data.Tuple

linearScanRegalloc :: BytecodeProgram -> BytecodeProgram
linearScanRegalloc = regalloc regMappings

data RAState = RAState
  { getAvailable :: [Int]
  , getActive :: SL.SortedList (ActiveLI, Int)
  , getMapping :: VRegMapping
  , getStackTop :: Int
  }

newtype ActiveLI = ActiveLI {getLI :: LiveInterval} deriving (Eq)

instance Ord ActiveLI where
  ActiveLI (_, a) `compare` ActiveLI (_, b) = a `compare` b

setMapping :: VRegMapping -> State RAState ()
setMapping m = modify $ \s -> s{getMapping = m}

setAvailable :: [Int] -> State RAState ()
setAvailable a = modify $ \s -> s{getAvailable = a}

setActive :: SL.SortedList (ActiveLI, Int) -> State RAState ()
setActive sl = modify $ \s -> s{getActive = sl}

setStackTop :: Int -> State RAState ()
setStackTop l = modify $ \s -> s{getStackTop = l}

getNextAvailable :: State RAState Integer
getNextAvailable = do
  result <- gets (toInteger . head . getAvailable)
  _ <- gets (setAvailable . tail . getAvailable)
  return result

addAvailable :: Integer -> State RAState ()
addAvailable i = gets getAvailable >>= setAvailable . (fromIntegral i :)

addMapping :: Integer -> VRegAssignment -> State RAState ()
addMapping virtual assignment =
  gets getMapping >>= setMapping . M.insert virtual assignment

addActive :: LiveInterval -> Int -> State RAState ()
addActive li i = gets getActive >>= setActive . SL.insert (ActiveLI li, i)

getNextLoc :: State RAState VRegAssignment
getNextLoc = do
  result <- gets getStackTop
  setStackTop (result + 1)
  return $ Spill result

lookupAssignment :: Integer -> State RAState VRegAssignment
lookupAssignment k = gets ((M.! k) . getMapping)

-- implementation of linear scan register allocation, taken directly from
-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
-- has a slight modification to deal with the precoloring specified by Plume's
-- calling convention
regMappings :: VRegIntervals -> [Int] -> VRegMapping
regMappings lis regPool =
  let sortedIntervals =
        sortBy (\a b -> compare (fst (snd a)) (fst (snd b))) (M.assocs lis)
      initState = RAState regPool (SL.toSortedList []) M.empty 0
      statefulProgram :: State RAState ()
      statefulProgram = mapM_ regallocIteration sortedIntervals
   in getMapping $ execState statefulProgram initState

regallocIteration :: (Integer, LiveInterval) -> State RAState ()
regallocIteration i@(virtual, li) = do
  expireOld i
  -- have not yet implemented spilling
  nActive <- gets (length . getActive)
  if nActive == numPhysical
    then spill i
    else do
      r <- getNextAvailable
      addMapping virtual $ Register (fromInteger r)
      addActive li (fromIntegral r)

expireOld :: (Integer, LiveInterval) -> State RAState ()
expireOld (_, li) = do
  actives <- gets (SL.map (first getLI) . getActive)
  let quitCond = (< fst li) <$> (snd . fst)
  let toProcess = SL.takeWhile quitCond actives
  setActive $ SL.map (first ActiveLI) $ SL.dropWhile quitCond actives
  mapM_ (addAvailable . toInteger . snd) toProcess

-- spilling isn't too complicated here -- just assign each reg its next place on the stack
spill :: (Integer, LiveInterval) -> State RAState ()
spill i@(virtual, li) = do
  actives <- gets (map (bimap getLI toInteger) . SL.fromSortedList . getActive)
  case actives of
    [] -> addMapping virtual =<< getNextLoc
    _ -> do
      let (sli, sr) = last actives
      if snd sli > snd li
        then do
          addMapping virtual =<< lookupAssignment sr
          addMapping (toInteger sr) =<< getNextLoc
          gets
            (SL.delete (ActiveLI sli, fromInteger sr) . getActive)
            >>= setActive
          gets
            (SL.insert (bimap ActiveLI fromInteger $ swap i) . getActive)
            >>= setActive
        else addMapping virtual =<< getNextLoc
