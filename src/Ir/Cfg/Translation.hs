module Ir.Cfg.Translation (toCfg, fromCfg) where

import Ir.Cfg.Types
import qualified Ir.Tac.Types as T

import qualified Data.Map.Strict as M
import Control.Monad.State

toCfg :: T.Program -> Program
toCfg = M.map func . T.getProgram

func :: T.Func -> Func
func (T.Func pts rt body) = Func pts rt undefined undefined

fromCfg :: Program -> T.Program
fromCfg = T.Program . M.map fromFunc

fromFunc :: Func -> T.Func
fromFunc (Func pts rt entry pool) = T.Func pts rt $ fromBlock pool entry

fromBlock :: BlockPool -> BasicBlock -> [T.Inst]
fromBlock pool (BasicBlock _ insts l out) =
  insts ++ fromEdgeAndLast pool out l

fromEdgeAndLast :: BlockPool -> Edge -> Last -> [T.Inst]
fromEdgeAndLast pool (If cons alt exit) (Cond pred_) =
  T.Cond pred_ consInsts altInsts : fromBlock pool (pool M.! exit)
  where
    consInsts = fromBlock pool $ pool M.! cons
    altInsts = fromBlock pool $ pool M.! alt
fromEdgeAndLast _ If {} _ = error "Encountered If edge without Cond last"
fromEdgeAndLast pool (Simple tag) Nil = fromBlock pool $ pool M.! tag
fromEdgeAndLast _ Simple {} _ = error "Encountered Simple edge without Nil last"
fromEdgeAndLast pool (Block body exit) Nil = fromBlock pool (pool M.! body) ++ fromBlock pool (pool M.! exit)
fromEdgeAndLast _ Block {} _ = error "Encountered Block edge without Nil last"
fromEdgeAndLast _ (ScopeEnd _) (Return mexpr) = [T.Return mexpr]
fromEdgeAndLast _ (ScopeEnd _) _ = []
