module VirtualMachine where

import BytecodeGen
import Control.Monad.State
import qualified Data.Map.Strict as M
import System.Exit

data VMState = VMState
  { getRegisters :: M.Map Integer Value,
    getCurrentProgram :: BytecodeProgram,
    getIPtr :: Integer,
    getRunning :: Bool
  }

setRegisters :: M.Map Integer Value -> State VMState ()
setRegisters new = modify $ \s -> s {getRegisters = new}

setRegister :: Integer -> Value -> State VMState ()
setRegister r v = do
  s <- get
  let cur = getRegisters s
  setRegisters (M.insert r v cur)

lookupRegister :: Integer -> VMState -> Value
lookupRegister r s = getRegisters s M.! r

addVal :: Value -> Value -> Value
addVal (VInt l) (VInt r) = VInt (l + r)
addVal (VFloat l) (VInt r) = VFloat (l + fromIntegral r)
addVal (VInt l) (VFloat r) = VFloat (fromIntegral l + r)
addVal (VFloat l) (VFloat r) = VFloat (l + r)

runBytecode :: BytecodeProgram -> IO ()
runBytecode b@(BytecodeProgram is tbl) =
  let start = tbl M.! "main"
      (rslt, _) = runState (runFrom start) (VMState M.empty b start True)
   in rslt

-- allows for program to dictate control flow, otherwise continues to next instruction
runFrom :: Integer -> State VMState (IO ())
runFrom start = do
  s <- get
  put $ s {getIPtr = start}
  result <-
    runInst $ getInstructions (getCurrentProgram s) !! fromIntegral (start - 1)
  s' <- get
  if getRunning s'
    then
      if getIPtr s' /= start
        then runFrom $ getIPtr s'
        else runFrom $ start + 1
    else return result

runInst :: Inst -> State VMState (IO ())
runInst (Move v (Register r)) = do
  s <- get
  case v of
    (Register src) -> setRegister r (lookupRegister src s)
    val -> setRegister r val
  return (pure ())
runInst (Add (Register l) (Register r) (Register dst)) = do
  s <- get
  let lval = lookupRegister l s
  let rval = lookupRegister r s
  setRegister dst (addVal lval rval)
  return (pure ())
runInst Ret = do
  s <- get
  put $ s {getRunning = False}
  case getRegisters s M.! retReg of
    (VInt 0) -> return exitSuccess
    (VInt v) -> return $ exitWith $ ExitFailure (fromIntegral v)
