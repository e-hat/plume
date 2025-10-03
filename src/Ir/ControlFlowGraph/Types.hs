module Ir.ControlFlowGraph.Types where 

import qualified Ir.ThreeAddressCode.Types as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Text.Printf (printf)

type Expr = T.Expr T.Term

type Label = String

data BasicBlock = BasicBlock Label [T.Inst] ControlFlow

blockSuccs :: BasicBlock -> S.Set Label
blockSuccs (BasicBlock _ _ (JumpIf _ l1 l2)) = S.fromList [l1, l2]
blockSuccs (BasicBlock _ _ (FuncCall _ l)) = S.singleton l
blockSuccs (BasicBlock _ _ (Jump l)) = S.singleton l
blockSuccs (BasicBlock _ _ Return) = S.empty

data ControlFlow 
    = JumpIf Expr Label Label
    | FuncCall T.Inst Label
    | Jump Label
    | Return

cfSuccs :: ControlFlow -> S.Set Label
cfSuccs (JumpIf _ a b) = S.fromList [a, b]
cfSuccs (FuncCall _ l) = S.singleton l
cfSuccs (Jump l) = S.singleton l
cfSuccs Return = S.empty

data Graph = Graph 
    { getBlocks :: M.Map Label BasicBlock
    , getSuccs :: M.Map Label (S.Set Label)
    , getPreds :: M.Map Label (S.Set Label)
    , getStart :: Label
    , getEnd :: Label}

-- Adding a new edge from block a to b
addEdge :: Graph -> Label -> Label -> Graph
addEdge g la lb = 
    let ssa = S.insert lb $ (getSuccs g) M.! la
        psb = S.insert la $ (getPreds g) M.! lb
        ss' = M.insert la ssa $ getSuccs g
        ps' = M.insert lb psb $ getPreds g
     in Graph (getBlocks g) ss' ps' (getStart g) (getEnd g)

insertBlock :: Graph -> BasicBlock -> Graph
insertBlock g b@(BasicBlock lb is f) = 
    let t' = M.insert lb b $ getBlocks g
        ss' = M.insert lb (cfSuccs f) (getSuccs g)
        -- The predecessors of this block are those for which the block is a successor
        psb = M.keysSet $ M.filter (\ssi -> S.member lb ssi) (getSuccs g)
        ps' = M.insert lb psb $ getPreds g
     in Graph t' ss' ps' (getStart g) (getEnd g)

fromList :: [BasicBlock] -> Graph
fromList [] = error "expected nonempty list"
fromList bs@((BasicBlock ns _ _):_) = 
    let (BasicBlock nf _ _) = last bs
        initialGraph = Graph M.empty M.empty M.empty ns nf 
        step :: Graph -> BasicBlock -> Graph
        step (Graph table succs preds start end) b@(BasicBlock name _ _) =
            let succsB = blockSuccs b
                updatePredStep :: M.Map Label (S.Set Label) -> Label -> M.Map Label (S.Set Label)
                updatePredStep accum succB = 
                    M.insert 
                        succB 
                        (S.insert name (M.findWithDefault S.empty succB accum)) 
                        accum
            in Graph 
                (M.insert name b table) 
                (M.insert name succsB succs) 
                (foldl updatePredStep preds succsB)
                start 
                end
     in foldl step initialGraph bs

showInsts :: [T.Inst] -> String
showInsts = intercalate "\n" . concatMap T.showInst

instance Show BasicBlock where
    show (BasicBlock name is out) = 
        case is of 
            [] -> printf ".%s\n%s" name (show out)
            _ -> printf ".%s:\n%s\n%s" 
                    name 
                    (showInsts is)
                    (show out)

instance Show ControlFlow where 
    show (JumpIf e ifTrue ifFalse) = 
        printf "if %s then GOTO .%s\n  else GOTO .%s" (show e) ifTrue ifFalse
    show (FuncCall i dst) = printf "%s\nGOTO .%s" (head (T.showInst i)) dst
    show (Jump l) = printf "GOTO .%s" l
    show Return = "END"

instance Show Graph where 
    show g = "cfg start:\n" 
        ++ fst (dfs S.empty (getStart g))
        where 
            dfs :: S.Set Label -> Label -> (String, S.Set Label)
            dfs s l
                | S.member l s = ("", s)
                | otherwise = 
                    let b@(BasicBlock name _ _) = (getBlocks g) M.! l
                        succs = (getSuccs g) M.! name
                        step :: (String, S.Set Label) -> Label -> (String, S.Set Label)
                        step (accum, s') succName = 
                            let (dfsResult, s'') = dfs s' succName
                             in if null dfsResult
                                then (accum, s'')
                                else (accum ++ "\n\n" ++ dfsResult, s'')
                        in foldl step (show b, S.insert name s) succs
                        
                    
