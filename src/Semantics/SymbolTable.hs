module Semantics.SymbolTable where

import Parsing.Syntax

import qualified Data.Map.Strict as Map

import Text.Show.Pretty

data TEntry = Single Type | Many [Type] Type deriving (Eq, Ord, Show)

-- symbol map for looking up during typechecking and beyond!
type SymTable = Map.Map Identifier TEntry

-- gets the symbol for a let or function definition declaration
getDeclSymbol :: DeclAug t -> Identifier
getDeclSymbol (Let _ i _, _) = i
getDeclSymbol (DefFn i _ _ _, _) = i
getDeclSymbol _ = error "getDeclSymbol used on non-let or non-deffn"

-- gets the type for a let or function definition declaration
getDeclEntry :: DeclAug t -> TEntry
getDeclEntry (Let t _ _, _) = Single t
getDeclEntry (DefFn _ ps r _, _) = Many (map (fst . getParam) ps) r
getDeclEntry _ = error "getDeclEntry used on non-let or non-deffn"

getEntryType :: TEntry -> Type
getEntryType (Single t) = t
getEntryType (Many _ t) = t

getDeclType :: DeclAug t -> Type
getDeclType d = getEntryType $ getDeclEntry d

lookupSymbolType :: Identifier -> SymTable -> Type
lookupSymbolType i tbl = getEntryType $ tbl Map.! i

getSymKV :: DeclAug t -> (Identifier, TEntry)
getSymKV (Let t i _, _) = (i, Single t)
getSymKV d@(DefFn i _ _ _, _) = (i, getDeclEntry d)
getSymKV _ = error "getSymKV used on non-let or non-deffn"

getParamKV :: Param -> (Identifier, TEntry)
getParamKV (Param ti) = (snd ti, Single $ fst ti)

insertParam :: Param -> SymTable -> SymTable
insertParam p = uncurry Map.insert (getParamKV p)

insertDecl :: DeclAug t -> SymTable -> SymTable
insertDecl d = uncurry Map.insert (getSymKV d)

data SymData = SymData
  { getScope :: SymTable
  , getSymSpan :: SpanRec
  }
  deriving (Show)

newtype SymDeclAug = SymDeclAug {getSymDeclAug :: DeclAug SymData}
newtype SymExprAug = SymExprAug {getSymExprAug :: ExprAug SymData}

newtype SymTreeList = SymTreeList {getSymTreeList :: [SymDeclAug]}

instance PrettyVal SymTreeList where
  prettyVal (SymTreeList ts) = prettyVal ts

instance PrettyVal SymDeclAug where
  prettyVal (SymDeclAug (Let t i e, SymData scp _)) = Con "Let" [String $ show scp, String t, String i, prettyVal $ SymExprAug e]
  prettyVal (SymDeclAug (Reassign i e, _)) = Con "Reassign" [String i, prettyVal $ SymExprAug e]
  prettyVal (SymDeclAug (DefFn i ps t e, SymData scp _)) =
    Con "DefFn" [String $ show scp, Con "FName" [String i], Con "Params" (map prettyVal ps), Con "Return type" [String t], Con "Body" [prettyVal $ SymExprAug e]]
  prettyVal (SymDeclAug (CallDecl i es, SymData scp _)) = Con "CallDecl" [String $ show scp, String i, Con "Params passed" [prettyVal $ map SymExprAug es]]
  prettyVal (SymDeclAug (IfDecl e d eds md, SymData scp _)) =
    Con "IfDecl" [String $ show scp, Con "Condition" [prettyVal $ SymExprAug e], Con "IfResult" [prettyVal $ SymDeclAug d], Con "ElseIfs" (map (prettyVal . augEFPair) eds), Con "Else" [prettyVal (SymDeclAug <$> md)]]
   where
    augEFPair (e', d') = (SymExprAug e', SymDeclAug d')
  prettyVal (SymDeclAug (BlockDecl ds, SymData scp _)) = Con "BlockDecl" [String $ show scp, prettyVal (map SymDeclAug ds)]

instance PrettyVal SymExprAug where
  prettyVal (SymExprAug (Subs i, SymData scp _)) = Con "Subs" [String $ show scp, String i]
  prettyVal (SymExprAug (CallExpr i es, SymData scp _)) = Con "CallExpr" [String $ show scp, String i, Con "Params passed" [prettyVal (map SymExprAug es)]]
  prettyVal (SymExprAug (IfExpr c e ees me, SymData scp _)) =
    Con "IfExpr" [String $ show scp, Con "Condition" [prettyVal $ SymExprAug c], Con "IfResult" [prettyVal $ SymExprAug e], Con "ElseIfs" (map (prettyVal . augEFPair) ees), Con "Else" [prettyVal (SymExprAug me)]]
   where
    augEFPair (e1, e2) = (SymExprAug e1, SymExprAug e2)
  prettyVal (SymExprAug (BlockExpr ds e, SymData scp _)) = Con "BlockExpr" [String $ show scp, prettyVal (map SymDeclAug ds), prettyVal $ SymExprAug e]
  prettyVal (SymExprAug (BinOp o a b, SymData scp _)) = Con "BinOp" [String $ show scp, String $ show o, prettyVal $ SymExprAug a, prettyVal $ SymExprAug b]
  prettyVal (SymExprAug (UnaryOp o a, SymData scp _)) = Con "UnaryOp" [String $ show scp, String $ show o, prettyVal $ SymExprAug a]
  prettyVal (SymExprAug (LitInt i, SymData{})) = Integer (show i)
  prettyVal (SymExprAug (LitBool b, SymData{})) = String (show b)
  prettyVal (SymExprAug (LitFloat f, SymData{})) = Float (show f)
  prettyVal (SymExprAug (LitString s, SymData{})) = String s
  prettyVal (SymExprAug (LitChar c, SymData{})) = Char (show c)
  prettyVal (SymExprAug (Return, SymData{})) = String "return"
