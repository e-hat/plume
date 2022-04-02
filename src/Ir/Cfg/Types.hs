module Ir.Cfg.Types where

import qualified Ir.Tac.Types as T

import qualified Data.Map.Strict as M

data BasicBlock = BasicBlock {getInsts :: [T.Inst], getIncoming :: Edge, getOutgoing :: Edge}

data Edge = Nil | Cond {getPred :: T.Expr T.Term, getCons :: BasicBlock, getAlt :: Maybe BasicBlock}

data Func = Func {getParamTypes :: [T.Type], getReturnType :: T.Type, getFunc :: BasicBlock}

type Program = M.Map String Func
