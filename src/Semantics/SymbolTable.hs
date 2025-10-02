module Semantics.SymbolTable where

import qualified Data.Map.Strict as Map
import Parsing.Syntax
import Text.Show.Pretty

data TEntry = Single Type | Many [Type] Type deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking and beyond!
type SymTable = Map.Map Identifier TEntry

-- gets the symbol for a let or function definition statement
getStmtSymbol :: StmtAug t -> Identifier
getStmtSymbol (Let _ i _, _) = i
getStmtSymbol (DefFn i _ _ _, _) = i
getStmtSymbol _ = error "getStmtSymbol used on non-let or non-deffn"

-- gets the type for a let or function definition statement
getStmtEntry :: StmtAug t -> TEntry
getStmtEntry (Let t _ _, _) = Single t
getStmtEntry (DefFn _ ps r _, _) = Many (map (fst . getParam) ps) r
getStmtEntry _ = error "getStmtEntry used on non-let or non-deffn"

getEntryType :: TEntry -> Type
getEntryType (Single t) = t
getEntryType (Many _ t) = t

getStmtType :: StmtAug t -> Type
getStmtType d = getEntryType $ getStmtEntry d

lookupSymbolType :: Identifier -> SymTable -> Type
lookupSymbolType i tbl = getEntryType $ tbl Map.! i

getSymKV :: StmtAug t -> (Identifier, TEntry)
getSymKV (Let t i _, _) = (i, Single t)
getSymKV d@(DefFn i _ _ _, _) = (i, getStmtEntry d)
getSymKV _ = error "getSymKV used on non-let or non-deffn"

getParamKV :: Param -> (Identifier, TEntry)
getParamKV (Param ti) = (snd ti, Single $ fst ti)

insertParam :: Param -> SymTable -> SymTable
insertParam p = uncurry Map.insert (getParamKV p)

insertStmt :: StmtAug t -> SymTable -> SymTable
insertStmt d = uncurry Map.insert (getSymKV d)

data SymData = SymData
    { getScope :: SymTable
    , getSymSpan :: SpanRec
    }
    deriving (Show)

newtype SymStmtAug = SymStmtAug {getSymStmtAug :: StmtAug SymData}

newtype SymExprAug = SymExprAug {getSymExprAug :: ExprAug SymData}

newtype SymTreeList = SymTreeList {getSymTreeList :: [SymStmtAug]}

instance PrettyVal SymTreeList where
    prettyVal (SymTreeList ts) = prettyVal ts

instance PrettyVal SymStmtAug where
    prettyVal (SymStmtAug (Let t i e, SymData scp _)) = Con "Let" [String $ show scp, String t, String i, prettyVal $ SymExprAug e]
    prettyVal (SymStmtAug (Reassign i e, _)) = Con "Reassign" [String i, prettyVal $ SymExprAug e]
    prettyVal (SymStmtAug (DefFn i ps t e, SymData scp _)) =
        Con "DefFn" [String $ show scp, Con "FName" [String i], Con "Params" (map prettyVal ps), Con "Return type" [String t], Con "Body" [prettyVal $ SymExprAug e]]
    prettyVal (SymStmtAug (CallStmt i es, SymData scp _)) = Con "CallStmt" [String $ show scp, String i, Con "Params passed" [prettyVal $ map SymExprAug es]]
    prettyVal (SymStmtAug (IfStmt e d eds md, SymData scp _)) =
        Con "IfStmt" [String $ show scp, Con "Condition" [prettyVal $ SymExprAug e], Con "IfResult" [prettyVal $ SymStmtAug d], Con "ElseIfs" (map (prettyVal . augEFPair) eds), Con "Else" [prettyVal (SymStmtAug <$> md)]]
      where
        augEFPair (e', d') = (SymExprAug e', SymStmtAug d')
    prettyVal (SymStmtAug (WhileStmt cond body, SymData scp _)) =
        Con "WhileStmt" [String $ show scp, Con "Condition" [prettyVal $ SymExprAug cond], Con "Body" [prettyVal $ SymStmtAug body]]
    prettyVal (SymStmtAug (BlockStmt ds, SymData scp _)) = Con "BlockStmt" [String $ show scp, prettyVal (map SymStmtAug ds)]

instance PrettyVal SymExprAug where
    prettyVal (SymExprAug (Subs i, SymData scp _)) = Con "Subs" [String $ show scp, String i]
    prettyVal (SymExprAug (CallExpr i es, SymData scp _)) = Con "CallExpr" [String $ show scp, String i, Con "Params passed" [prettyVal (map SymExprAug es)]]
    prettyVal (SymExprAug (IfExpr c e ees me, SymData scp _)) =
        Con "IfExpr" [String $ show scp, Con "Condition" [prettyVal $ SymExprAug c], Con "IfResult" [prettyVal $ SymExprAug e], Con "ElseIfs" (map (prettyVal . augEFPair) ees), Con "Else" [prettyVal (SymExprAug me)]]
      where
        augEFPair (e1, e2) = (SymExprAug e1, SymExprAug e2)
    prettyVal (SymExprAug (BlockExpr ds e, SymData scp _)) = Con "BlockExpr" [String $ show scp, prettyVal (map SymStmtAug ds), prettyVal $ SymExprAug e]
    prettyVal (SymExprAug (BinOp o a b, SymData scp _)) = Con "BinOp" [String $ show scp, String $ show o, prettyVal $ SymExprAug a, prettyVal $ SymExprAug b]
    prettyVal (SymExprAug (UnaryOp o a, SymData scp _)) = Con "UnaryOp" [String $ show scp, String $ show o, prettyVal $ SymExprAug a]
    prettyVal (SymExprAug (LitInt i, SymData{})) = Integer (show i)
    prettyVal (SymExprAug (LitBool b, SymData{})) = String (show b)
    prettyVal (SymExprAug (LitFloat f, SymData{})) = Float (show f)
    prettyVal (SymExprAug (LitString s, SymData{})) = String s
    prettyVal (SymExprAug (LitChar c, SymData{})) = Char (show c)
    prettyVal (SymExprAug (Return, SymData{})) = String "return"
