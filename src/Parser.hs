module Parser where

import Control.Monad
import qualified Lexer as L
import Syntax
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as Ex

program :: P.Parsec String () Program
program = Program <$> (L.whiteSpace >> P.many1 declaration)

-- wraps an declaration parser to keep track of its span
declWrapper :: P.Parsec String () Decl -> P.Parsec String () DeclNode
declWrapper declP = do
  start <- P.getPosition
  val <- declP
  end <- P.getPosition
  return $ Node (spanBtwnSP start end) val

exprWrapper :: P.Parsec String () Expr -> P.Parsec String() ExprNode 
exprWrapper exprP = do
  start <- P.getPosition 
  val <- exprP
  end <- P.getPosition 
  return $ Node (spanBtwnSP start end) val

----------------------------------------------------------
------------------- arithmetic expressions ---------------
----------------------------------------------------------
asExprNode :: Expr -> ExprNode
asExprNode b@(BinOp _ n1 n2) = Node (getSpan n1 <> getSpan n2) b
asExprNode u@(UnaryOp _ n) = Node (getSpan n) u

binary s f =
  Ex.Infix $ do
    L.reservedOp s
    return (\x y -> asExprNode (BinOp f x y))

unary s f =
  Ex.Prefix $ do
    pos <- P.getPosition
    L.reservedOp s
    return (asExprNode . UnaryOp f)

aOpTable =
  [ [unary "-" Neg]
  , [ binary "*" Mul Ex.AssocLeft
    , binary "/" Divide Ex.AssocLeft
    , binary "//" IntDivide Ex.AssocLeft
    ]
  , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
  ]

aExpression :: P.Parsec String () ExprNode
aExpression = Ex.buildExpressionParser aOpTable aTerm

aTerm =
  L.parens aExpression P.<|> P.try callexpr P.<|> P.try subs P.<|> int P.<|> float

--------------------------------------------------------
--------------- boolean expressions---------------------
--------------------------------------------------------
bOpTable =
  [ [unary "not" Not]
  , [binary "and" And Ex.AssocLeft]
  , [binary "or" Or Ex.AssocLeft]
  ]

bExpression :: P.Parsec String () ExprNode
bExpression = Ex.buildExpressionParser bOpTable bTerm

bTerm =
  L.parens bExpression P.<|> 
    P.try rExpression P.<|>
  P.try callexpr P.<|> P.try subs P.<|>
  bool 

---------------------------------------------------------
-------------- relational expressions -------------------
---------------------------------------------------------
rExpression =
  exprWrapper $ do
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
int :: P.Parsec String () ExprNode
int = exprWrapper $ LitInt <$> L.integer

float :: P.Parsec String () ExprNode
float = exprWrapper $ LitFloat <$> L.float

string :: P.Parsec String () ExprNode
string =
  exprWrapper $ do
    P.char '\"'
    contents <- L.string
    P.char '\"'
    return $ LitString contents

bool :: P.Parsec String () ExprNode
bool =
  exprWrapper $
  (L.reserved "true" >> return (LitBool True)) P.<|>
  (L.reserved "false" >> return (LitBool False))

char :: P.Parsec String () ExprNode
char =
  exprWrapper $ do
    P.char '\''
    contents <- L.char
    P.char '\''
    return $ LitChar contents

-----------------------------------------------------------
------------------more complex parsing---------------------
-----------------------------------------------------------
-- the expressions themselves
-- "trys" will be optimized after everything else so that I have behavior to test against
declaration :: P.Parsec String () DeclNode
declaration =
  P.try letdecl P.<|> P.try deffn P.<|>
  P.try calldecl P.<|>
  P.try reassign P.<|>
  P.try ifdecl P.<|>
  P.try elseifdecl P.<|>
  P.try elsedecl P.<|>
  P.try blockdecl P.<?>
  "a declaration (something without a result)"

expression :: P.Parsec String () ExprNode 
expression =
  P.try bExpression
  P.<|> P.try aExpression
  P.<|> P.try callexpr
  P.<|> P.try subs
  P.<|> P.try ifexpr
  P.<|> P.try elseifexpr
  P.<|> P.try elseexpr
  P.<|> P.try blockexpr
  P.<|> P.try int
  P.<|> P.try float
  P.<|> P.try string
  P.<|> P.try bool
  P.<|> P.try char
  P.<?> "an expression (something that has a result)"

letdecl :: P.Parsec String () DeclNode
letdecl =
  declWrapper $ do
    t <- L.typeName
    ident <- L.identifier
    L.reservedOp ":="
    Let t ident <$> expression

reassign :: P.Parsec String () DeclNode
reassign =
  declWrapper $ do
    ident <- L.identifier
    L.reservedOp "<-"
    Reassign ident <$> expression

subs :: P.Parsec String () ExprNode
subs = exprWrapper $ Subs <$> L.identifier

deffn :: P.Parsec String () DeclNode
deffn =
  declWrapper $ do
    L.reserved "def"
    ident <- L.identifier
    params <- L.parens $ P.sepBy L.param (L.reservedOp ",")
    L.reservedOp ":"
    returnType <- L.typeName
    L.reservedOp ":="
    DefFn ident params returnType <$> expression

callexpr :: P.Parsec String () ExprNode 
callexpr =
  exprWrapper $ do
    ident <- L.identifier 
    params <- L.parens $ P.sepBy expression (L.reservedOp ",")
    return $ CallExpr ident params

declFromExpr :: ExprNode -> Decl 
declFromExpr (Node _ e) = 
  makeCall e
    where
      makeCall :: Expr -> Decl
      makeCall (CallExpr i exs) = CallDecl i exs 

calldecl :: P.Parsec String () DeclNode
calldecl =
  declWrapper $ do
    stmt <- callexpr
    L.reservedOp ";"
    return $ declFromExpr stmt

ifexpr :: P.Parsec String () ExprNode
ifexpr =
  exprWrapper $ do
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    IfExpr cond <$> expression

ifdecl :: P.Parsec String () DeclNode 
ifdecl =
  declWrapper $ do
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    IfDecl cond <$> declaration 

elseifexpr :: P.Parsec String () ExprNode
elseifexpr =
  exprWrapper $ do
    L.reserved "else"
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    ElseIfExpr cond <$> expression

elseifdecl :: P.Parsec String () DeclNode 
elseifdecl =
  declWrapper $ do
    L.reserved "else"
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    ElseIfDecl cond <$> declaration

elseexpr :: P.Parsec String () ExprNode
elseexpr =
  exprWrapper $ do
    L.reserved "else"
    L.reservedOp "=>"
    ElseExpr <$> expression

elsedecl :: P.Parsec String () DeclNode 
elsedecl =
  declWrapper $ do
    L.reserved "else"
    L.reservedOp "=>"
    ElseDecl <$> declaration

blockexpr :: P.Parsec String () ExprNode
blockexpr = 
  exprWrapper $ do
    decls <- L.braces $ P.many declaration
    BlockExpr decls <$> expression

blockdecl :: P.Parsec String () DeclNode 
blockdecl =
  declWrapper $ L.braces $ BlockDecl <$> P.many declaration
