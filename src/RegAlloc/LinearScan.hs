module RegAlloc.LinearScan (regalloc) where

import Bytecode.Types
import qualified Data.Map.Strict as M
import RegAlloc.LiveIntervals

import Control.Monad.State
import Data.Bifunctor
import Data.List
import qualified Data.Set as S
import qualified Data.SortedList as SL

data VRegAssignment = Register Int | Spill Int
type VRegMapping = M.Map Integer VRegAssignment

regalloc :: BytecodeProgram -> BytecodeProgram
regalloc p@(BytecodeProgram _ ltbl) =
  let frms = M.intersectionWith ($) (M.map regMappings (liveIntervals p)) regPools
      rebuildFromFuncs :: BytecodeFuncs -> [Inst]
      rebuildFromFuncs fm = concatMap snd fl
       where
        fl = sortBy (\(f1, _) (f2, _) -> compare (ltbl M.! f1) (ltbl M.! f2)) (M.assocs fm)
      fs = funcs p
      regPools = M.map regPool fs
      regPool :: [Inst] -> [Int]
      regPool is =
        S.toList $
          S.fromAscList [1 .. numPhysical] S.\\ usedPhysicals is
   in p{getInstructions = rebuildFromFuncs $ M.intersectionWith convertToPhysical frms fs}

numPhysical :: Int
numPhysical = 5

usedPhysicals :: [Inst] -> S.Set Int
usedPhysicals = foldl handleInst (S.fromList [])
  where 
    handleInst :: S.Set Int -> Inst -> S.Set Int
    handleInst accum (Move v1 v2) = doubleVal accum [v1, v2]
    handleInst accum (Add v1 v2) =  doubleVal accum [v1, v2] 
    handleInst accum (Sub v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (Mul v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (Div v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (Neg v) = singleVal accum v 
    handleInst accum (IAnd v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (IOr v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (Inv v) = singleVal accum v 
    handleInst accum (Cmp v1 v2) = doubleVal accum [v1, v2] 
    handleInst accum (Push v) = singleVal accum v 
    handleInst accum (Pop v) = singleVal accum v
    handleInst accum _ = accum
    handleValue :: S.Set Int -> Value -> S.Set Int
    handleValue accum (PRegister r) = S.insert (fromInteger r) accum
    handleValue accum _ = accum
    doubleVal :: S.Set Int -> [Value] ->  S.Set Int
    doubleVal = foldl handleValue
    singleVal :: S.Set Int -> Value -> S.Set Int 
    singleVal = handleValue 

convertToPhysical :: VRegMapping -> [Inst] -> [Inst]
convertToPhysical rm = map updateRs
 where
  u :: Value -> Value
  u (VRegister v) = 
    case rm M.! v of 
      Register p -> PRegister $ toInteger p
      Spill l -> StackLoc l
  u v = v
  updateRs :: Inst -> Inst
  updateRs (Move v1 v2) = Move (u v1) (u v2)
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
addActive li i =
  gets getActive >>= setActive . SL.insert (ActiveLI li, i)

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
spill = undefined

