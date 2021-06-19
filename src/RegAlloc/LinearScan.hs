module RegAlloc.LinearScan (regalloc) where

import RegAlloc.LiveIntervals
import Bytecode.Types
import qualified Data.Map.Strict as M
import Data.List
import qualified Data.SortedList as SL
import Control.Monad.State
import Data.Bifunctor

regalloc :: BytecodeProgram -> BytecodeProgram
regalloc p@(BytecodeProgram _ ltbl) = 
  let frm = M.map (regMappings numPhysical) (liveIntervals p)
      rebuildFromFuncs :: M.Map Label [Inst] -> [Inst] 
      rebuildFromFuncs fm = concatMap snd fl
        where fl = sortBy (\(f1, _) (f2, _) -> compare (ltbl M.! f1) (ltbl M.! f2)) (M.assocs fm) 
   in p {getInstructions = rebuildFromFuncs $ M.intersectionWith convertToPhysical frm (funcs p)}

numPhysical :: Int 
numPhysical = 5

convertToPhysical :: M.Map Integer Integer -> [Inst] -> [Inst]
convertToPhysical rm = map updateRs
  where 
    u :: Value -> Value 
    u (VRegister r) = VRegister $ rm M.! r
    u v = v
    updateRs :: Inst -> Inst
    updateRs Ret = Ret
    updateRs (Add v1 v2) = Add (u v1) (u v2)
    updateRs (Sub v1 v2) = Sub (u v1) (u v2)
    updateRs (Mul v1 v2) = Mul (u v1) (u v2)
    updateRs (Div v1 v2) = Div (u v1) (u v2)
    updateRs (Neg v) = Neg (u v)
    updateRs (IAnd v1 v2) = IAnd (u v1) (u v2)
    updateRs (IOr v1 v2) = IOr (u v1) (u v2)
    updateRs (Inv v) = Inv (u v)
    updateRs (Cmp v1 v2) = Cmp (u v1) (u v2)
    updateRs (Push v) = Push (u v)
    updateRs (Pop v) = Pop (u v)
    updateRs other = other

data RAState = RAState
  { getAvailable :: [Int]
  , getActive :: SL.SortedList (ActiveLI, Int)
  , getMapping :: M.Map Integer Integer
  }

newtype ActiveLI = ActiveLI { getLI :: LiveInterval } deriving Eq

instance Ord ActiveLI where 
  ActiveLI (_, a) `compare` ActiveLI (_, b) = a `compare` b

setMapping :: M.Map Integer Integer -> State RAState ()
setMapping m = modify $ \s -> s {getMapping = m}

setAvailable :: [Int] -> State RAState ()
setAvailable a = modify $ \s -> s {getAvailable = a}

setActive :: SL.SortedList (ActiveLI, Int) -> State RAState ()
setActive sl = modify $ \s -> s {getActive = sl}

getNextAvailable :: State RAState Integer
getNextAvailable = do 
  result <- gets (toInteger . head . getAvailable)
  _ <- gets (setAvailable . tail . getAvailable)
  return result

addAvailable :: Integer -> State RAState ()
addAvailable i = gets getAvailable >>= setAvailable . (fromIntegral i :)

addMapping :: Integer -> Integer -> State RAState ()
addMapping virtual physical = 
  gets getMapping >>= setMapping . M.insert virtual physical

addActive :: LiveInterval -> Int -> State RAState ()
addActive li i= 
  gets getActive >>= setActive . SL.insert (ActiveLI li, i)

-- implementation of linear scan register allocation, taken directly from 
-- http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
regMappings :: Int -> M.Map Integer LiveInterval -> M.Map Integer Integer
regMappings n lis = 
  let sortedIntervals = 
        sortBy (\a b -> compare (fst (snd a)) (fst (snd b))) (M.assocs lis)
      initState = RAState [1..n] (SL.toSortedList []) M.empty
      statefulProgram :: State RAState ()
      statefulProgram = mapM_ regallocIteration sortedIntervals
  in getMapping $ execState statefulProgram initState

regallocIteration :: (Integer, LiveInterval) -> State RAState ()
regallocIteration i@(virtual, li) = do 
  expireOld i
  -- have not yet implemented spilling
  r <- getNextAvailable
  addMapping virtual r
  addActive li (fromIntegral r)

expireOld :: (Integer, LiveInterval) -> State RAState ()
expireOld (_, li) = do 
  actives <- gets (SL.map (first getLI) . getActive)
  let quitCond = (< fst li) <$> (snd . fst)
  let toProcess = SL.takeWhile quitCond actives
  setActive $ SL.map (first ActiveLI) $ SL.dropWhile quitCond actives
  mapM_ (addAvailable . toInteger . snd) toProcess

spill :: (Integer, LiveInterval) -> State RAState ()
spill = undefined
