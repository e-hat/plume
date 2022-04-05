module Ir.Cfg.Translation (toCfg, fromCfg) where

import Ir.Cfg.Types
import qualified Ir.Tac.Types as T

import qualified Data.Map.Strict as M

toCfg :: T.Program -> Program
toCfg = undefined

fromCfg :: Program -> T.Program
fromCfg = T.Program . M.map fromCfgFunc

fromCfgFunc :: Func -> T.Func
fromCfgFunc (Func pts rt blk) = T.Func pts rt $ fromBlk blk

fromBlk :: BasicBlock -> [T.Inst]
fromBlk (BasicBlock insts _ out) = insts ++ maybe [] fromEdge out

fromEdge :: Edge -> [T.Inst]
fromEdge (Cond _ p cons mAlt) = [T.Cond p (fromBlk cons) (maybe [] fromBlk mAlt)]
fromEdge (Simple _ child) = [T.Block $ fromBlk child]
