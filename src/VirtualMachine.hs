module VirtualMachine where

import ShowBytecode
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

evaluateValue :: Value -> ()
evaluateValue (VInt i) = seq i ()
evaluateValue (VFloat f) = seq f ()
evaluateValue (Register t) = seq t ()
evaluateValue (VBool b) = seq b ()
evaluateValue (VByte b) = seq b ()

setRegister :: Integer -> Value -> State VMState ()
setRegister r v = do
  s <- get
  let cur = getRegisters s
  evaluateValue v `seq` setRegisters (M.insert r v cur)

lookupRegister :: Integer -> VMState -> Value
lookupRegister r s = getRegisters s M.! r

type BinArithOp = forall a. Num a => a -> a -> a

arithComb :: VMState -> BinArithOp -> Value -> Value -> Value
arithComb _ op (VInt l) (VInt r) = VInt (op l r)
arithComb _ op (VFloat l) (VInt r) = VFloat (op l (fromIntegral r))
arithComb _ op (VInt l) (VFloat r) = VFloat (op (fromIntegral l) r)
arithComb _ op (VFloat l) (VFloat r) = VFloat (op l r)
arithComb s op (Register lr) r =
  let l = lookupRegister lr s
   in arithComb s op l r
arithComb s op l (Register rr) =
  let r = lookupRegister rr s
   in arithComb s op l r

divComb :: VMState -> Value -> Value -> Value
divComb _ (VInt l) (VInt r) = VInt (l `quot` r)
divComb _ (VFloat l) (VInt 0) = error "Divide by zero"
divComb _ (VFloat l) (VInt r) = VFloat (l / fromIntegral r)
divComb _ (VInt l) (VFloat 0.0) = error "Divide by zero"
divComb _ (VInt l) (VFloat r) = VFloat (fromIntegral l / r)
divComb _ (VFloat l) (VFloat r) = VFloat (l / r)
divComb s (Register lr) r =
  let l = lookupRegister lr s
   in divComb s l r
divComb s l (Register rr) =
  let r = lookupRegister rr s
   in divComb s l r

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
    Register src -> setRegister r (lookupRegister src s)
    val -> setRegister r val
  return (pure ())
runInst (Add l r dst) = runBinArithInst (+) l r dst
runInst (Sub l r dst) = runBinArithInst (-) l r dst
runInst (Mul l r dst) = runBinArithInst (*) l r dst
runInst (Div l r (Register dst)) = do
  s <- get
  setRegister dst (divComb s l r)
  return (pure ())
runInst (Neg v (Register dst)) = do 
  s <- get 
  setRegister dst (negateVal s v)
  return (pure ())
    where 
      negateVal :: VMState -> Value -> Value
      negateVal _ (VFloat v) = VFloat (negate v)
      negateVal _ (VInt v) = VInt (negate v)
      negateVal s (Register t) = 
        let v = lookupRegister t s
         in negateVal s v
runInst Ret = do
  s <- get
  put $ s {getRunning = False}
  case getRegisters s M.! retReg of
    VInt 0 -> return exitSuccess
    VInt v -> return $ exitWith $ ExitFailure (fromIntegral v)

runBinArithInst :: BinArithOp -> Value -> Value -> Value -> State VMState (IO ())
runBinArithInst op l r (Register dst) = do
  s <- get
  setRegister dst (arithComb s op l r)
  return (pure ())
