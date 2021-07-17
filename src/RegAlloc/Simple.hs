module RegAlloc.Simple (simpleRegalloc) where

import Bytecode.Types
import RegAlloc.Shared

import Control.Monad.State
import qualified Data.Map.Strict as M

simpleRegalloc :: BytecodeProgram -> BytecodeProgram
simpleRegalloc = regalloc regMappings

data RAState = RAState
  { getMapping :: VRegMapping
  , getAvailableRs :: [Int]
  , getStackTop :: Int
  }

getNextAssignment :: State RAState VRegAssignment
getNextAssignment = do
  ars <- gets getAvailableRs
  case ars of
    [] -> do
      retVal <- gets getStackTop
      modify $ \s -> s{getStackTop = retVal + 1}
      return $ Spill retVal
    r : rs -> do
      modify $ \s -> s{getAvailableRs = rs}
      return $ Register r

regMappings :: [Inst] -> [Int] -> VRegMapping
regMappings is rs =
  let initState = RAState M.empty rs 1
      statefulProgram = mapM_ simpleIteration is
   in getMapping $ execState statefulProgram initState

simpleIteration :: Inst -> State RAState ()
simpleIteration = handleInst
  where 
    handleInst :: Inst -> State RAState () 
    handleInst (Move v1 v2) = handle2 (v1, v2)
    handleInst (Add v1 v2) = handle2 (v1, v2) 
    handleInst (Sub v1 v2) = handle2 (v1, v2)
    handleInst (Mul v1 v2) = handle2 (v1, v2) 
    handleInst (Div v1 v2) = handle2 (v1, v2)
    handleInst (Neg v) = handle1 v
    handleInst (IAnd v1 v2) = handle2 (v1, v2) 
    handleInst (IOr v1 v2) = handle2 (v1, v2)
    handleInst (Inv v) = handle1 v 
    handleInst (Cmp v1 v2) = handle2 (v1, v2) 
    handleInst (Push v) = handle1 v
    handleInst (Pop v) = handle1 v
    handleInst _ = return ()
    handle2 :: (Value, Value) -> State RAState ()
    handle2 (v1, v2) = handle1 v1 >> handle1 v2
    handle1 :: Value -> State RAState ()
    handle1 (VRegister r) = do 
      m <- gets getMapping 
      case M.lookup r m of 
        Just _ -> return ()
        Nothing -> do
          a <- getNextAssignment 
          modify $ \s -> s{getMapping = M.insert r a m}
    handle1 _ = return ()

          



