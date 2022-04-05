module Ir.Cfg.Types where

import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T

data BasicBlock = BasicBlock {getInsts :: [T.Inst], getIncoming :: Maybe Edge, getOutgoing :: Maybe Edge}

data Edge
  = Cond
      { getCondParent :: BasicBlock,
        getPred :: T.Expr T.Term,
        getCons :: BasicBlock,
        getAlt :: Maybe BasicBlock
      }
  | Simple {getSimpleParent :: BasicBlock, getSimpleNext :: BasicBlock}

data Func = Func {getParamTypes :: [T.Type], getReturnType :: T.Type, getFunc :: BasicBlock}

type Program = M.Map String Func

getParent :: BasicBlock -> Maybe BasicBlock
getParent (BasicBlock _ mIncoming _) = getParentEdge <$> mIncoming

getParentEdge :: Edge -> BasicBlock
getParentEdge (Cond p _ _ _) = p
getParentEdge (Simple p _) = p
