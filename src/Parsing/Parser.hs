{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parsing.Parser where

import Control.Monad
import qualified Parsing.Lexer as L
import Parsing.Syntax
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as Ex

program :: P.Parsec String () Program
program = Program . map ASTStmtAug <$> (L.whiteSpace *> P.many1 (P.try letstmt P.<|> P.try deffn) <* P.eof)

-- wraps an stmtaration parser to keep track of its span
stmtWrapper :: P.Parsec String () (Stmt SpanRec) -> P.Parsec String () (StmtAug SpanRec)
stmtWrapper stmtP = do
    start <- P.getPosition
    val <- stmtP
    end <- P.getPosition
    return (val, spanBtwnSP start end)

exprWrapper :: P.Parsec String () (Expr SpanRec) -> P.Parsec String () (ExprAug SpanRec)
exprWrapper exprP = do
    start <- P.getPosition
    val <- exprP
    end <- P.getPosition
    return (val, spanBtwnSP start end)

--------------------------------------------------------------
------------------- expressions with operators ---------------
--------------------------------------------------------------
-- Plume has all of its expressions with binary/unary ops
-- parsed as one type, then errors are figured out in typechecking-land

asExprAug :: Expr SpanRec -> ExprAug SpanRec
asExprAug b@(BinOp _ n1 n2) = (b, snd n1 <> snd n2)
asExprAug u@(UnaryOp _ n) = (u, snd n)
asExprAug _ = undefined

binary s f =
    Ex.Infix $ do
        L.reservedOp s
        return (\x y -> asExprAug (BinOp f x y))

unary s f =
    Ex.Prefix $ do
        L.reservedOp s
        return (asExprAug . UnaryOp f)

opTable =
    [ [unary "-" Negate]
    ,
        [ binary "*" Multiply Ex.AssocLeft
        , binary "/" Divide Ex.AssocLeft
        ]
    , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
    ,
        [ binary "<" Less Ex.AssocLeft
        , binary ">" Greater Ex.AssocLeft
        , binary "<=" Leq Ex.AssocLeft
        , binary ">=" Geq Ex.AssocLeft
        , binary "=" Equal Ex.AssocLeft
        , binary "!=" NotEqual Ex.AssocLeft
        ]
    , [unary "not" Not]
    , [binary "and" And Ex.AssocLeft]
    , [binary "or" Or Ex.AssocLeft]
    ]

opExpression :: P.Parsec String () (ExprAug SpanRec)
opExpression = Ex.buildExpressionParser opTable term

term :: P.Parsec String () (ExprAug SpanRec)
term =
    L.parens opExpression P.<|> P.try callexpr P.<|> P.try subs P.<|> natOrFloat P.<|> bool

------------------------------------------------------
------------------literal parsing---------------------
------------------------------------------------------

natOrFloat :: P.Parsec String () (ExprAug SpanRec)
natOrFloat = exprWrapper $ do
    num <- L.natOrFloat
    case num of
        Left i -> return $ LitInt i
        Right f -> return $ LitFloat f

string :: P.Parsec String () (ExprAug SpanRec)
string = exprWrapper $ LitString <$> L.string

bool :: P.Parsec String () (ExprAug SpanRec)
bool =
    exprWrapper $
        (L.reserved "true" >> return (LitBool True))
            P.<|> (L.reserved "false" >> return (LitBool False))

char :: P.Parsec String () (ExprAug SpanRec)
char =
    exprWrapper $ LitChar <$> L.char

-----------------------------------------------------------
------------------more complex parsing---------------------
-----------------------------------------------------------
-- the expressions themselves
-- "trys" will be optimized after everything else so that I have behavior to test against
stmtaration :: P.Parsec String () (StmtAug SpanRec)
stmtaration =
    P.try letstmt P.<|> P.try deffn
        P.<|> P.try callstmt
        P.<|> P.try reassign
        P.<|> P.try ifstmt
        P.<|> P.try whilestmt
        P.<|> P.try blockstmt
        P.<?> "a stmtaration (something without a result)"

-- not allowed to define functions in if statements and etc
bodyStatement :: P.Parsec String () (StmtAug SpanRec)
bodyStatement =
    P.try letstmt
        P.<|> P.try callstmt
        P.<|> P.try reassign
        P.<|> P.try ifstmt
        P.<|> P.try whilestmt
        P.<|> P.try blockstmt
        P.<|> P.try letstmt
        P.<?> "a stmtaration (something without a result)"

expression :: P.Parsec String () (ExprAug SpanRec)
expression =
    P.try opExpression
        P.<|> P.try subs
        P.<|> P.try callexpr
        P.<|> P.try ifexpr
        P.<|> P.try blockexpr
        P.<|> P.try natOrFloat
        P.<|> P.try string
        P.<|> P.try bool
        P.<|> P.try char
        P.<|> P.try returnexpr
        P.<?> "an expression (something that has a result)"

yieldexpr :: P.Parsec String () (ExprAug SpanRec)
yieldexpr = L.reserved "yield" >> expression

letstmt :: P.Parsec String () (StmtAug SpanRec)
letstmt =
    stmtWrapper $ do
        t <- L.typeName
        -- this shouldn't be allowed
        guard (t /= "Void")
        ident <- L.identifier
        L.reservedOp ":="
        Let t ident <$> expression

reassign :: P.Parsec String () (StmtAug SpanRec)
reassign =
    stmtWrapper $ do
        ident <- L.identifier
        L.reservedOp "<-"
        Reassign ident <$> expression

subs :: P.Parsec String () (ExprAug SpanRec)
subs = exprWrapper $ Subs <$> L.identifier

deffn :: P.Parsec String () (StmtAug SpanRec)
deffn =
    stmtWrapper $ do
        L.reserved "def"
        ident <- P.try L.identifier P.<|> (L.reserved "main" >> return "main")
        params <- L.parens $ P.sepBy L.param (L.reservedOp ",")
        L.reservedOp ":"
        returnType <- L.typeName
        L.reservedOp ":="
        DefFn ident (map Param params) returnType <$> expression

paramexpr :: P.Parsec String () (ExprAug SpanRec)
paramexpr =
    P.try opExpression
        P.<|> P.try subs
        P.<|> P.try callexpr
        P.<|> P.try natOrFloat
        P.<|> P.try string
        P.<|> P.try bool
        P.<|> P.try char

callexpr :: P.Parsec String () (ExprAug SpanRec)
callexpr =
    exprWrapper $ do
        ident <- L.identifier
        params <- L.parens $ P.sepBy paramexpr (L.reservedOp ",")
        return $ CallExpr ident params

stmtFromExpr :: ExprAug SpanRec -> Stmt SpanRec
stmtFromExpr (e, _) = makeCall e
  where
    makeCall :: Expr SpanRec -> Stmt SpanRec
    makeCall (CallExpr i exs) = CallStmt i exs
    makeCall _ = undefined

callstmt :: P.Parsec String () (StmtAug SpanRec)
callstmt =
    stmtWrapper $ do
        stmt <- callexpr
        L.reservedOp ";"
        return $ stmtFromExpr stmt

ifbodyexpr :: P.Parsec String () (ExprAug SpanRec)
ifbodyexpr =
    P.try opExpression
        P.<|> P.try subs
        P.<|> P.try callexpr
        P.<|> P.try ifexpr
        P.<|> P.try blockexpr
        P.<|> P.try natOrFloat
        P.<|> P.try string
        P.<|> P.try bool
        P.<|> P.try char
        P.<|> P.try returnexpr

ifexpr :: P.Parsec String () (ExprAug SpanRec)
ifexpr =
    exprWrapper $ do
        L.reserved "if"
        cond <- opExpression
        L.reservedOp "=>"
        firstexpr <- ifbodyexpr
        elseifs <- P.many $ P.try elseifexpr
        elsecase <- P.try elseexpr
        return $ IfExpr cond firstexpr elseifs elsecase

ifstmt :: P.Parsec String () (StmtAug SpanRec)
ifstmt =
    stmtWrapper $ do
        L.reserved "if"
        cond <- opExpression
        L.reservedOp "=>"
        firststmt <- bodyStatement
        elseifs <- P.many (P.try elseifstmt)
        elsecase <- P.optionMaybe (P.try elsestmt)
        return $ IfStmt cond firststmt elseifs elsecase

elseifexpr :: P.Parsec String () (ExprAug SpanRec, ExprAug SpanRec)
elseifexpr =
    do
        L.reserved "else"
        L.reserved "if"
        cond <- opExpression
        L.reservedOp "=>"
        (,) cond <$> ifbodyexpr

elseifstmt :: P.Parsec String () (ExprAug SpanRec, StmtAug SpanRec)
elseifstmt =
    do
        L.reserved "else"
        L.reserved "if"
        cond <- opExpression
        L.reservedOp "=>"
        (,) cond <$> bodyStatement

elseexpr :: P.Parsec String () (ExprAug SpanRec)
elseexpr =
    do
        L.reserved "else"
        L.reservedOp "=>"
        ifbodyexpr

elsestmt :: P.Parsec String () (StmtAug SpanRec)
elsestmt =
    do
        L.reserved "else"
        L.reservedOp "=>"
        bodyStatement

whilestmt :: P.Parsec String () (StmtAug SpanRec)
whilestmt =
    stmtWrapper $ do
        L.reserved "while"
        cond <- opExpression
        L.reservedOp "=>"
        WhileStmt cond <$> bodyStatement

blockreturnexpr :: P.Parsec String () (ExprAug SpanRec)
blockreturnexpr =
    P.try yieldexpr
        P.<|> P.try opExpression
        P.<|> P.try subs
        P.<|> P.try callexpr
        P.<|> P.try ifexpr
        P.<|> P.try natOrFloat
        P.<|> P.try string
        P.<|> P.try bool
        P.<|> P.try char
        P.<|> P.try returnexpr

blockexpr :: P.Parsec String () (ExprAug SpanRec)
blockexpr =
    exprWrapper . L.braces $ do
        stmts <- P.many bodyStatement
        BlockExpr stmts <$> blockreturnexpr

blockstmt :: P.Parsec String () (StmtAug SpanRec)
blockstmt =
    stmtWrapper . L.braces $ BlockStmt <$> P.many bodyStatement

returnexpr :: P.Parsec String () (ExprAug SpanRec)
returnexpr = exprWrapper $ L.reserved "return" >> return Return
