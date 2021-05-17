module BytecodeGen
  ( Inst (..),
    Value (..),
    Label (..),
    BytecodeProgram (..),
    SyscallCode (..),
    genBytecode,
    retReg,
  )
where

import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as M
import SymbolTable
import Syntax
import Text.Printf (errorShortFormat, printf)

-- types of values that can be moved around
data Value
  = Register Integer
  | VBool Bool
  | VInt Integer
  | VFloat Double
  | VByte Char
  | SyscallCode SyscallCode 

data SyscallCode = Exit deriving Show

data Inst
  = Ret
  | Move Value Value
  | Add Value Value
  | Sub Value Value
  | Mul Value Value
  | Div Value Value
  | Neg Value
  | IAnd Value Value
  | IOr Value Value
  | Inv Value
  | Cmp Value Value
  | Jmp String
  | JmpNotEqual String
  | JmpEqual String
  | JmpGeq String
  | JmpLeq String
  | JmpL String
  | JmpG String
  | Push Value 
  | Pop Value
  | Call String
  | Syscall

data Label = FuncLabel String | JmpLabel String deriving (Eq, Ord)

data BytecodeProgram = BytecodeProgram
  { getInstructions :: [Inst],
    getLabelTable :: M.Map Label Integer
  }

data GState = GState
  { getCurrentProgram :: BytecodeProgram,
    getOpenRegisters :: [Integer],
    getVarRegisters :: M.Map String Integer,
    getGlobalVars :: M.Map String Value,
    getOpenLabelNums :: [Integer]
  } 

-- list of registers that it uses, then SyscallCode
data SyscallSchema = SyscallSchema [Integer] SyscallCode

-- pre-defined schema section
exitSchema = SyscallSchema [1] Exit

-- helper functions for managing state
setCurrentProgram :: BytecodeProgram -> State GState ()
setCurrentProgram b = modify $ \s -> s {getCurrentProgram = b}

appendInst :: Inst -> State GState ()
appendInst i = do
  prog <- gets getCurrentProgram
  setCurrentProgram $ prog {getInstructions = getInstructions prog ++ [i]}

appendLabel :: Label -> State GState ()
appendLabel l = do
  prog <- gets getCurrentProgram
  let tbl = getLabelTable prog
  let loc = toInteger (length (getInstructions prog))
  setCurrentProgram $ prog {getLabelTable = M.insert l loc tbl}

appendFuncLabel :: String -> State GState ()
appendFuncLabel = appendLabel . FuncLabel 

appendJmpLabel :: String -> State GState ()
appendJmpLabel = appendLabel . JmpLabel

setOpenRegisters :: [Integer] -> State GState ()
setOpenRegisters rs = modify $ \s -> s {getOpenRegisters = rs}

setOpenLabelNums :: [Integer] -> State GState ()
setOpenLabelNums ls = modify $ \s -> s {getOpenLabelNums = ls}

setVarRegisters :: M.Map String Integer -> State GState ()
setVarRegisters vr = modify $ \s -> s {getVarRegisters = vr}

setVarRegister :: String -> Integer -> State GState ()
setVarRegister i r = gets getVarRegisters >>= setVarRegisters . M.insert i r

setGlobalVars :: M.Map String Value -> State GState ()
setGlobalVars gv = modify $ \s -> s {getGlobalVars = gv}

getNextRegister :: State GState Integer
getNextRegister = do
  rs <- gets getOpenRegisters
  let result = head rs
  setOpenRegisters $ tail rs
  return result

prettifyLabel :: Integer -> String
prettifyLabel = printf "*%06d*"

getNextLabel :: State GState String
getNextLabel = do
  ls <- gets getOpenLabelNums
  let result = head ls
  setOpenLabelNums $ tail ls
  return (prettifyLabel result)

-- equivalent of %eax in this bytecode is $0
retReg :: Integer
retReg = 0

initState :: GState
initState = GState (BytecodeProgram [] M.empty) [retReg + 1 ..] M.empty M.empty [1 ..]

ensureMinRegister :: Integer -> State GState ()
ensureMinRegister i = do 
  i' <- gets (head . getOpenRegisters)
  setOpenRegisters [max i i'..]

-------------------------------------------------------------------------------
----------------------------- BYTECODE GENERATION -----------------------------
-------------------------------------------------------------------------------
genBytecode :: SymTreeList -> BytecodeProgram
genBytecode trees =
  let statefulProgram =
        traverse (genGlobalTree . getSymDeclAug) (getSymTreeList trees)
      (_, result) = runState statefulProgram initState
   in getCurrentProgram result

-- this will need to be rewritten using memory constructs instead
addGlobalVar :: String -> Value -> State GState ()
addGlobalVar i v = gets getGlobalVars >>= setGlobalVars . M.insert i v

genGlobalTree :: DeclAug SymData -> State GState ()
genGlobalTree (Let _ i (LitInt v, _), _) = addGlobalVar i (VInt v)
genGlobalTree (Let _ i (LitBool v, _), _) = addGlobalVar i (VBool v)
genGlobalTree (Let _ i (LitChar v, _), _) = addGlobalVar i (VByte v)
genGlobalTree (Let _ i (LitFloat v, _), _) = addGlobalVar i (VFloat v)
-- current main function signature
genGlobalTree (DefFn "main" [] "Int" e, _) = do 
  appendFuncLabel "main"
  genSyscall [e] exitSchema
genGlobalTree (DefFn i ps "Void" e, _) = do
  appendFuncLabel i
  setupParams ps
  genVoidExpr e
genGlobalTree (DefFn i ps _ e, _) = do
  appendFuncLabel i
  setupParams ps
  moveExprInto retReg e
  appendInst Ret

genSyscall :: [ExprAug SymData] -> SyscallSchema -> State GState ()
genSyscall es (SyscallSchema rs code) = do 
  -- save hardcoded register values
  mapM_ (appendInst . Push . Register) rs
  -- make sure the rest of code generation is "aware" of the hardcoded register 
  -- values being used
  ensureMinRegister (foldl max (-1) rs + 1)
  -- this is VERY similar to how a function call works, this is moving the params 
  -- into the designated registers
  zipWithM_ moveExprInto rs es   
  appendInst $ Push (Register 0)
  appendInst $ Move (SyscallCode code) (Register 0)
  appendInst Syscall
  appendInst $ Pop (Register 0)
  mapM_ (appendInst . Pop . Register) (reverse rs)

-----------------------------------------------------------------------------
---------------------CALLING CONVENTION--------------------------------------
-----------------------------------------------------------------------------
-- This is how the calling convention works: Params are stored in 
-- registers $1, $2, ...
-- Caller pushes the values currently held in $1, $2, ... onto the stack
-- Caller puts parameter values into registers $1, $2, ...
-- Caller calls callee (that's a fun sentence)
-- Callee now has its parameters in registers $1, $2, ...
-- Callee pushes all of the registers it uses in its body onto the stack 
-- Callee does it's thing! (By this, I mean it executes)
-- Callee pops the values it saved at the beginning into their original registers
-- Callee puts result, if applicable, into $0, and returns
-- Caller moves result out of $0 into its dest 
-- Caller pops all of the saved parameter registers back into $1, $2, ...
-- Done!!
setupParams :: [Param] -> State GState ()
setupParams ps = do
  -- map each parameter name to a register, starting at $1
  zipWithM_ (\p r -> setVarRegister (snd $ getParam p) r) ps [1 ..]
  lowestOpenReg <- gets (head . getOpenRegisters)
  let maxParamRegBound = toInteger $ length ps + 1
  -- make sure the rest of the generated code is aware that this happened "unnaturally"
  setOpenRegisters [max lowestOpenReg maxParamRegBound ..]

genDecl :: DeclAug SymData -> State GState ()
genDecl (Let _ i e, _) = do
  r <- getNextRegister
  moveExprInto r e
  setVarRegister i r
genDecl (Reassign i e, _) = do
  rvs <- gets getVarRegisters
  case M.lookup i rvs of
    Just r -> moveExprInto r e
    Nothing -> error "I need to implement memory to make reassignments of global variables work!"
genDecl (BlockDecl ds, _) = traverse_ genDecl ds
genDecl (IfDecl ic ie eifs me, _) =
  let genCE :: String -> [(ExprAug SymData, DeclAug SymData)] -> State GState ()
      genCE exit [(cond, body)] =
        case me of
          Just e -> do
            elseLbl <- getNextLabel
            genConditional cond (genDecl body) elseLbl
            appendInst (Jmp exit)
            appendJmpLabel elseLbl
            genDecl e
          Nothing -> do
            genConditional cond (genDecl body) exit
      genCE exit ((cond, body) : rest) = do
        nextCond <- getNextLabel
        genConditional cond (genDecl body) nextCond
        appendInst (Jmp exit)
        appendJmpLabel nextCond
        genCE exit rest
   in do
        exit <- getNextLabel
        genCE exit ((ic, ie) : eifs)
        appendJmpLabel exit
genDecl (CallDecl i args, _) = do 
  appendInst (Push (Register retReg))
  let argRegs = [1..toInteger $ length args]
  traverse_ (appendInst . Push . Register) argRegs
  zipWithM_ moveExprInto argRegs args
  appendInst (Call i)
  traverse_ (appendInst . Pop . Register) (reverse argRegs)
  appendInst (Pop (Register retReg))

genDecl _ = error "haven't implemented this yet"

-- puts the result of an expression into the specified register
--
-- in a way, this function is central to Plume, as the syntax allows 
-- variables/functions to have entire if statement expressions assigned to them
moveExprInto :: Integer -> ExprAug SymData -> State GState ()
moveExprInto t (BlockExpr ds e, _) = do
  traverse_ genDecl ds
  moveExprInto t e
-- I don't love how this works. Is it the most important problem I am facing? No.
moveExprInto t b@(BinOp Leq l r, _) = moveRelInto t b
moveExprInto t b@(BinOp Less l r, _) = moveRelInto t b
moveExprInto t b@(BinOp Geq l r, _) = moveRelInto t b
moveExprInto t b@(BinOp Greater l r, _) = moveRelInto t b
moveExprInto t b@(BinOp Equal l r, _) = moveRelInto t b
moveExprInto t b@(BinOp NotEqual l r, _) = moveRelInto t b
moveExprInto t b@(BinOp _ l r, _) = do
  lval <- genExprValue l
  rval <- genExprValue r
  appendInst (Move rval (Register t))
  appendInst (binOpMapping b lval (Register t))
moveExprInto t u@(UnaryOp Negate e, _) = do
  val <- genExprValue e
  appendInst (Move val (Register t))
  appendInst (Neg (Register t))
moveExprInto t u@(UnaryOp Not e, _) = do
  val <- genExprValue e
  appendInst (Move val (Register t))
  appendInst (Inv (Register t))
moveExprInto t i@(IfExpr {}, _) = genIfElseStructure (moveExprInto t) i
moveExprInto t (CallExpr i args, _) = do 
  let usedRegs = filter (/= t) (retReg : [1..toInteger $ length args])
  let argRegs = [1..toInteger $ length args]
  traverse_ (appendInst . Push . Register) usedRegs
  zipWithM_ moveExprInto argRegs args
  appendInst (Call i)
  appendInst (Move (Register retReg) (Register t))
  traverse_ (appendInst . Pop . Register) (reverse usedRegs)
moveExprInto t e = do
  v <- genExprValue e
  appendInst (Move v (Register t))

-- this function takes a condition, a body to execute if that condition is
-- true, and a place to jump to if that condition is false. Equivalent to:
-- if cond => body
--
-- IMPORTANT NOTE: it leaves the control flow of the bytecode INSIDE the body
-- of the statement, after the last instruction. Therefore, It is UP TO THE CALLER to
-- control what the code does after it executes the body
--
-- Along the same lines, it takes a `fail` label as input but it is UP TO THE CALLER 
-- to decide where to append said `fail` label in the bytecode 
genConditional :: ExprAug SymData -> State GState () -> String -> State GState ()
genConditional cond body fail
  | isRel cond =
    let (BinOp _ l r, _) = cond
     in do
          lval <- genExprValue l
          rval <- genExprValue r
          appendInst (Cmp lval rval)
          appendInst (relOpInverseMapping cond fail)
          body
  | otherwise = do
    cresult <- genExprValue cond
    appendInst (Cmp cresult (VBool True))
    appendInst (JmpNotEqual fail)
    body

genIfElseStructure :: (ExprAug SymData -> State GState ()) -> ExprAug SymData -> State GState ()
genIfElseStructure bodyGen i@(IfExpr ic ie eifs ee, _) =
  let genCE :: String -> String -> (ExprAug SymData, ExprAug SymData) -> State GState ()
      genCE exit nextCond (cond, body) = do
        genConditional cond (bodyGen body) nextCond
        appendInst (Jmp exit)
        appendJmpLabel nextCond
   in do
        elseLbl <- getNextLabel
        eifLbls <- replicateM (1 + length eifs) getNextLabel
        exit <- getNextLabel
        zipWithM_ (genCE exit) (eifLbls ++ [elseLbl]) ((ic, ie) : eifs)
        -- now dealing with else case
        bodyGen ee
        appendJmpLabel exit

moveRelInto :: Integer -> ExprAug SymData -> State GState ()
moveRelInto t rel = do
  false <- getNextLabel
  exit <- getNextLabel
  genConditional rel (appendInst (Move (VBool True) (Register t))) false
  appendInst (Jmp exit)
  appendJmpLabel false
  appendInst (Move (VBool False) (Register t))
  appendJmpLabel exit

genExprValue :: ExprAug SymData -> State GState Value
genExprValue (LitInt v, _) = return (VInt v)
genExprValue (LitBool v, _) = return (VBool v)
genExprValue (LitChar v, _) = return (VByte v)
genExprValue (LitFloat v, _) = return (VFloat v)
-- Subs will become far more complicated with memory
genExprValue (Subs i, _) = do
  rvs <- gets getVarRegisters 
  case M.lookup i rvs of
    Just reg -> return (Register reg)
    Nothing -> do
      gvs <- gets getGlobalVars
      case M.lookup i gvs of
        Nothing -> error $ "ERROR: SYMBOL " ++ i ++ " CANNOT BE FOUND"
        Just v -> return v
genExprValue (Return, _) = appendInst Ret >> return (VInt 0)
genExprValue e = do
  n <- getNextRegister
  moveExprInto n e
  return (Register n)

genVoidExpr :: ExprAug SymData -> State GState ()
genVoidExpr (Return, _) = appendInst Ret
genVoidExpr (BlockExpr ds e, _) = do
  traverse_ genDecl ds
  genVoidExpr e
genVoidExpr i@(IfExpr {}, _) = genIfElseStructure genVoidExpr i

-- these mappings allow for dynamic creation of some simple instructions
-- to avoid annoying boilerplate
binOpMapping :: ExprAug SymData -> (Value -> Value -> Inst)
binOpMapping (BinOp Plus _ _, _) = Add
binOpMapping (BinOp Minus _ _, _) = Sub
binOpMapping (BinOp Multiply _ _, _) = Mul
binOpMapping (BinOp Divide _ _, _) = Div
binOpMapping (BinOp And _ _, _) = IAnd
binOpMapping (BinOp Or _ _, _) = IOr

-- used in if statements/other conditionals to greatly reduce the
-- number of instructions generated by jumping directly based on the result
-- of a comparison, rather than computing the comparison than comparing the
-- result to True
relOpInverseMapping :: ExprAug SymData -> (String -> Inst)
relOpInverseMapping (BinOp Leq _ _, _) = JmpG
relOpInverseMapping (BinOp Less _ _, _) = JmpGeq
relOpInverseMapping (BinOp Geq _ _, _) = JmpL
relOpInverseMapping (BinOp Greater _ _, _) = JmpLeq
relOpInverseMapping (BinOp Equal _ _, _) = JmpNotEqual
relOpInverseMapping (BinOp NotEqual _ _, _) = JmpEqual

-- helps differentiate relationals so that if statements with relationals
-- as the root node of the condition can be generated more efficiently
isRel :: ExprAug SymData -> Bool
isRel (BinOp Leq _ _, _) = True
isRel (BinOp Less _ _, _) = True
isRel (BinOp Geq _ _, _) = True
isRel (BinOp Greater _ _, _) = True
isRel (BinOp Equal _ _, _) = True
isRel (BinOp NotEqual _ _, _) = True
isRel _ = False
