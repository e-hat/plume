module VirtualMachine where

import BytecodeGen
import System.Exit
import Control.Monad.State
import qualified Data.Map.Strict as M

data VMState = VMState 
  { getRegisters :: M.Map Integer Value,
    getCurrentProgram :: BytecodeProgram,
    getIPtr :: Integer,
    getRunning :: Bool
  } 

setRegisters :: M.Map Integer Value -> State VMState ()
setRegisters new = modify $ \s -> s { getRegisters = new }

setRegister :: Integer -> Value -> State VMState ()
setRegister r v = do 
  s <- get 
  let cur = getRegisters s 
  setRegisters (M.insert r v cur)

runBytecode :: BytecodeProgram -> IO ()
runBytecode b@(BytecodeProgram is tbl) = 
  let start = tbl M.! "main"
      (rslt, _) = runState (runFrom start) (VMState M.empty b start True)
   in rslt

runFrom :: Integer -> State VMState (IO ())
runFrom start = do
  s <- get
  put $ s { getIPtr = start }
  result <- 
    runInst $ getInstructions (getCurrentProgram s) !! fromIntegral (start - 1)
  s' <- get
  if getRunning s' 
    then if getIPtr s' /= start 
             then runFrom $ getIPtr s' 
             else runFrom $ start + 1
    else return result

runInst :: Inst -> State VMState (IO ())
runInst (Move v (Register r)) = do 
  s <- get 
  let rs = getRegisters s
  case v of 
    (Register src) -> setRegister r (rs M.! src)
    val -> setRegister r val
  return (pure ())
runInst Ret = do
  s <- get 
  put $ s { getRunning = False }
  case getRegisters s M.! retReg of 
    (VInt 0) -> return exitSuccess 
    (VInt v) -> return $ exitWith $ ExitFailure (fromIntegral v)
