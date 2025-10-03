module Ir.Cfg.Types where 

import qualified Ir.Tac.Types as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Text.Printf (printf)

type Expr = T.Expr T.Term

type Label = String

data BasicBlock = BasicBlock Label [T.Inst] ControlFlow

blockSuccs :: BasicBlock -> [Label]
blockSuccs (BasicBlock _ _ (JumpIf _ l1 l2)) = [l1, l2]
blockSuccs (BasicBlock _ _ (FuncCall _ l)) = [l]
blockSuccs (BasicBlock _ _ (Jump l)) = [l]
blockSuccs (BasicBlock _ _ Return) = []

data ControlFlow 
    = JumpIf Expr Label Label
    | FuncCall T.Inst Label
    | Jump Label
    | Return

data Cfg = Cfg 
    { getBlockTable :: M.Map Label BasicBlock
    , getSuccs :: M.Map Label [Label]
    , getPreds :: M.Map Label [Label]
    , getStart :: Label
    , getEnd :: Label}

fromList :: [BasicBlock] -> Cfg
fromList [] = error "expected nonempty list"
fromList bs@((BasicBlock ns _ _):_) = 
    let (BasicBlock nf _ _) = last bs
        initialCfg = Cfg M.empty M.empty M.empty ns nf 
        step :: Cfg -> BasicBlock -> Cfg
        step (Cfg table succs preds start end) b@(BasicBlock name _ _) =
            let succsB = blockSuccs b
                updatePredStep :: M.Map Label [Label] -> Label -> M.Map Label [Label]
                updatePredStep accum succB = 
                    M.insert 
                        succB 
                        ((M.findWithDefault [] succB accum) ++ [name]) 
                        accum
            in Cfg 
                (M.insert name b table) 
                (M.insert name succsB succs) 
                (foldl updatePredStep preds succsB)
                start 
                end
     in foldl step initialCfg bs

showInsts :: [T.Inst] -> String
showInsts = intercalate "\n" . concatMap T.showInst

instance Show BasicBlock where
    show (BasicBlock name is out) = 
        printf ".%s:\n%s\n%s" 
            (show name) 
            (showInsts is)
            (show out)

instance Show ControlFlow where 
    show (JumpIf e ifTrue ifFalse) = 
        printf "if %s then GOTO .%s\n  else GOTO .%s" (show e) ifTrue ifFalse
    show (FuncCall i dst) = printf "%s\nGOTO .%s" (head (T.showInst i)) dst
    show (Jump l) = printf "GOTO .%s" l
    show Return = "END"

instance Show Cfg where 
    show cfg = "cfg start:\n" 
        ++ fst (dfs S.empty (getStart cfg))
        where 
            dfs :: S.Set Label -> Label -> (String, S.Set Label)
            dfs s l
                | S.member l s = ("", s)
                | otherwise = 
                    let b@(BasicBlock name _ _) = (getBlockTable cfg) M.! l
                        succs = (getSuccs cfg) M.! name
                        step :: (String, S.Set Label) -> Label -> (String, S.Set Label)
                        step (accum, s') succName = 
                            let (dfsResult, s'') = dfs s' succName
                             in if null dfsResult
                                then (accum, s'')
                                else (accum ++ "\n\n" ++ dfsResult, s'')
                        in foldl step (show b, S.insert name s) succs
                        
                    
