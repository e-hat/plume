module Ir.Cfg.Types where

import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T

type Id = String

data BasicBlock = BasicBlock
  { getId :: Id,
    getInsts :: [T.Inst],
    getLast :: Last,
    getOutgoing :: Edge
  }

data Edge
  = If {getCons :: Id, getAlt :: Id, getExit :: Id}
  | Simple Id
  | Block {getBody :: Id, getExit :: Id}
  | ScopeEnd Id

data Last
  = Cond {getPred :: T.Expr T.Term}
  | Return (Maybe (T.Expr T.Term))
  | Nil

type BlockPool = M.Map String BasicBlock

data Func = Func {getParamTypes :: [T.Type], getReturnType :: T.Type, getEntryBlock :: BasicBlock, getPool :: BlockPool}

type Program = M.Map String Func
