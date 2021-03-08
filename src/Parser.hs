module Parser where

import Control.Monad
import qualified Lexer as L
import Syntax
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as Ex

-- wraps an expression parser to keep track of its span
spanWrapper :: P.Parsec String () Expr -> P.Parsec String () ASTNode
spanWrapper exprP = do
  start <- P.getPosition
  val <- exprP
  end <- P.getPosition
  return $ ASTNode (spanBtwnSP start end) val

----------------------------------------------------------
------------------- arithmetic expressions ---------------
----------------------------------------------------------
asAST :: Expr -> ASTNode
asAST b@(BinOp _ n1 n2) = ASTNode (getSpan n1 <> getSpan n2) b
asAST u@(UnaryOp _ n) = ASTNode (getSpan n) u

binary s f =
  Ex.Infix $ do
    L.reservedOp s
    return (\x y -> asAST (BinOp f x y))

unary s f =
  Ex.Prefix $ do
    pos <- P.getPosition
    L.reservedOp s
    return (asAST . UnaryOp f)

aOpTable =
  [ [unary "-" Neg]
  , [ binary "*" Mul Ex.AssocLeft
    , binary "/" Divide Ex.AssocLeft
    , binary "//" IntDivide Ex.AssocLeft
    ]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  ]

aExpression :: P.Parsec String () ASTNode
aExpression = Ex.buildExpressionParser aOpTable aTerm

aTerm =
  L.parens aExpression P.<|> P.try call P.<|> P.try subs P.<|> int P.<|> float

--------------------------------------------------------
--------------- boolean expressions---------------------
--------------------------------------------------------
bOpTable =
  [ [unary "not" Not]
  , [binary "and" And Ex.AssocLeft]
  , [binary "or" Or Ex.AssocLeft]
  ]

bExpression :: P.Parsec String () ASTNode
bExpression = Ex.buildExpressionParser bOpTable bTerm

bTerm =
  L.parens bExpression P.<|> P.try call P.<|> reassign P.<|> P.try subs P.<|>
  bool P.<|>
  rExpression

---------------------------------------------------------
-------------- relational expressions -------------------
---------------------------------------------------------
rExpression = spanWrapper $ do
  a1 <- aExpression
  r <- relation
  BinOp r a1 <$> aExpression

relation =
  (L.reservedOp "<" >> return Less) P.<|> (L.reservedOp "<=" >> return Leq) P.<|>
  (L.reservedOp ">" >> return Greater) P.<|>
  (L.reservedOp ">=" >> return Geq) P.<|>
  (L.reservedOp "=" >> return Equal) P.<|>
  (L.reservedOp "!=" >> return NotEqual)

------------------------------------------------------
------------------literal parsing---------------------
------------------------------------------------------
int :: P.Parsec String () ASTNode
int = spanWrapper $ LitInt <$> L.integer

float :: P.Parsec String () ASTNode
float = spanWrapper $ LitFloat <$> L.float

string :: P.Parsec String () ASTNode
string =
  spanWrapper $ do
    P.char '\"'
    contents <- L.string
    P.char '\"'
    return $ LitString contents

bool :: P.Parsec String () ASTNode
bool =
  spanWrapper $
  (L.reserved "true" >> return (LitBool True)) P.<|>
  (L.reserved "false" >> return (LitBool False))

char :: P.Parsec String () ASTNode
char =
  spanWrapper $ do
    P.char '\''
    contents <- L.char
    P.char '\''
    return $ LitChar contents

semicolon :: P.Parsec String () ASTNode
semicolon = spanWrapper $ L.semicolon >> return Semicolon

-----------------------------------------------------------
------------------more complex parsing---------------------
-----------------------------------------------------------
-- the expressions themselves
-- "trys" will be optimized after everything else so that I have behavior to test against
expression :: P.Parsec String () ASTNode
expression =
  P.try letexpr P.<|> P.try deffn P.<|> P.try bExpression P.<|>
  P.try aExpression P.<|>
  P.try call P.<|>
  P.try reassign P.<|>
  P.try subs P.<|>
  P.try semicolon P.<|>
  P.try ifexpr P.<|>
  P.try elseif P.<|>
  P.try elseexpr P.<|>
  P.try semicolon P.<|>
  P.try block

letexpr :: P.Parsec String () ASTNode
letexpr =
  spanWrapper $ do
    t <- L.typeName
    ident <- L.identifier
    L.reservedOp ":="
    Let t ident <$> expression

reassign :: P.Parsec String () ASTNode
reassign =
  spanWrapper $ do
    ident <- L.identifier
    L.reservedOp "<-"
    Reassign ident <$> expression

subs :: P.Parsec String () ASTNode
subs = spanWrapper $ Subs <$> L.identifier

deffn :: P.Parsec String () ASTNode
deffn =
  spanWrapper $ do
    L.reserved "def"
    ident <- L.identifier
    params <- L.parens $ P.sepBy L.param (L.reservedOp ",")
    L.reservedOp ":"
    returnType <- L.typeName
    L.reservedOp ":="
    DefFn ident params returnType <$> expression

call :: P.Parsec String () ASTNode
call =
  spanWrapper $ do
    ident <- L.identifier
    params <- L.parens $ P.sepBy expression (L.reservedOp ",")
    return $ Call ident params

ifexpr :: P.Parsec String () ASTNode
ifexpr =
  spanWrapper $ do
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    If cond <$> expression

elseif :: P.Parsec String () ASTNode
elseif =
  spanWrapper $ do
    L.reserved "else"
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    ElseIf cond <$> expression

elseexpr :: P.Parsec String () ASTNode
elseexpr =
  spanWrapper $ do
    L.reserved "else"
    L.reservedOp "=>"
    Else <$> expression

block :: P.Parsec String () ASTNode
block = spanWrapper $ Block <$> L.braces (P.many1 expression)
