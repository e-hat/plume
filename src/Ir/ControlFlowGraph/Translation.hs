module Ir.ControlFlowGraph.Translation (fromFunc) where 

import Ir.ControlFlowGraph.Types
import qualified Ir.ThreeAddressCode.Types as T
import Control.Monad.State
import qualified Data.Map.Strict as M

data TState = TState 
    { getCurrentBlock :: BasicBlock
    , getGraph :: Graph
    , getLabelCounter :: Int
    , getExitStack :: [Label]
    }

appendInst :: T.Inst -> State TState ()
appendInst i = modify $ \s -> 
    let (BasicBlock name is f) = getCurrentBlock s
     in s{getCurrentBlock = BasicBlock name (is ++ [i]) f}

setControlFlow :: ControlFlow -> State TState ()
setControlFlow f = do 
    (BasicBlock name is _) <- gets getCurrentBlock
    modify $ \s -> s{getCurrentBlock = BasicBlock name is f}

writeBlock :: State TState ()
writeBlock = do 
    b <- gets getCurrentBlock
    g <- gets getGraph
    modify $ \s -> s{getGraph = insertBlock g b}

resetBlock :: Label -> State TState ()
resetBlock l = modify $ \s -> s{getCurrentBlock = BasicBlock l [] Return}

nextLabel :: State TState Label
nextLabel = do 
    n <- gets getLabelCounter
    modify $ \s -> s{getLabelCounter = n + 1}
    return $ "L" ++ (show n)

exitStackPop :: State TState Label
exitStackPop = do 
    lls <- gets getExitStack
    let l = head lls
    let ls = tail lls
    modify $ \s -> s{getExitStack = ls}
    return l

exitStackTop :: State TState Label
exitStackTop = do 
    lls <- gets getExitStack
    return $ head lls

startLabel :: String
startLabel = "start"

endLabel :: String
endLabel = "end"

fromFunc :: T.Func -> Graph
fromFunc f = 
    let b0 = BasicBlock startLabel [] Return
        c0 = Graph M.empty M.empty M.empty startLabel endLabel
        s0 = TState b0 c0 1 []
     in getGraph $ execState (func f) s0

func :: T.Func -> State TState ()
func (T.Func _ tret is) = do 
    resetBlock startLabel
    mapM_ tac is 
    setControlFlow (Jump endLabel)
    writeBlock
    resetBlock endLabel
    writeBlock

tac :: T.Inst -> State TState ()
tac i@(T.Assignment _ rhs) = do
    appendInst i
    case rhs of 
        T.FuncCallExpr {} -> do
            dst <- nextLabel
            setControlFlow (Jump dst)
            writeBlock
            resetBlock dst
        _ -> return ()
tac i@(T.Cond p c a) = do error "unimplemented"
tac i@(T.While p is) = do error "unimplemented"
tac i@(T.Block is) = do 
    mapM_ tac is
tac i@(T.IgnoreReturnValCall _) = do 
    appendInst i
    lf <- nextLabel
    setControlFlow (Jump lf)
    writeBlock
    resetBlock lf
tac i@(T.Return Nothing) = do error "unimplemented"
tac i@(T.Return {}) = do 
    appendInst i
    setControlFlow (Jump endLabel)
    writeBlock
    l <- nextLabel
    resetBlock l
