module Parser where

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as Ex

import Control.Monad

import qualified Lexer as L
import Syntax

binary s f = Ex.Infix (L.reservedOp s >> return (BinOp f))
unary  s f = Ex.Prefix (L.reservedOp s >> return (UnaryOp f))

----------------------------------------------------------
------------------- arithmetic expressions ---------------
----------------------------------------------------------
aOpTable = [[unary  "-" Not                                                                                  ]
           ,[binary "*" Mul  Ex.AssocLeft, binary "/" Divide Ex.AssocLeft, binary "//" IntDivide Ex.AssocLeft]
           ,[binary "+" Plus Ex.AssocLeft, binary "-" Minus  Ex.AssocLeft                                    ]]


aExpression :: P.Parsec String () Expr
aExpression = Ex.buildExpressionParser aOpTable aTerm

aTerm =   L.parens aExpression
    P.<|> P.try call
    P.<|> P.try subs
    P.<|> int
    P.<|> float

--------------------------------------------------------
--------------- boolean expressions---------------------
--------------------------------------------------------
bOpTable = [[unary  "not" Not             ]
           ,[binary "and" And Ex.AssocLeft]
           ,[binary "or"  Or  Ex.AssocLeft]]

bExpression :: P.Parsec String () Expr
bExpression = Ex.buildExpressionParser bOpTable bTerm

bTerm =   L.parens bExpression
    P.<|> P.try call
    P.<|> reassign
    P.<|> P.try subs
    P.<|> bool
    P.<|> rExpression

---------------------------------------------------------
-------------- relational expressions -------------------
---------------------------------------------------------

rExpression = do
  a1 <- aExpression
  r <- relation
  BinOp r a1 <$> aExpression


relation =  (L.reservedOp "<"  >> return Less    )
      P.<|> (L.reservedOp "<=" >> return Leq     )
      P.<|> (L.reservedOp ">"  >> return Greater )
      P.<|> (L.reservedOp ">=" >> return Geq     )
      P.<|> (L.reservedOp "="  >> return Equal   )
      P.<|> (L.reservedOp "!=" >> return NotEqual)

------------------------------------------------------
------------------literal parsing---------------------
------------------------------------------------------

int :: P.Parsec String () Expr
int = do
  LitInt <$> L.integer 

float :: P.Parsec String () Expr 
float = do
  LitFloat <$> L.float

string :: P.Parsec String () Expr 
string = do
  P.char '\"'
  contents <- L.string
  P.char '\"'
  return $ LitString contents

bool :: P.Parsec String () Expr
bool =  (L.reserved "true"  >> return (LitBool True )) 
  P.<|> (L.reserved "false" >> return (LitBool False))

char :: P.Parsec String () Expr 
char = do
  P.char '\''
  contents <- L.char 
  P.char '\''
  return $ LitChar contents

semicolon :: P.Parsec String () Expr
semicolon = L.semicolon >> return Semicolon

-----------------------------------------------------------
------------------more complex parsing---------------------
-----------------------------------------------------------

-- the expressions themselves
-- "trys" will be optimized after everything else so that I have behavior to test against
expression :: P.Parsec String () Expr 
expression 
  =     P.try letexpr
  P.<|> P.try deffn
  P.<|> P.try bExpression
  P.<|> P.try aExpression
  P.<|> P.try call
  P.<|> P.try reassign
  P.<|> P.try subs
  P.<|> P.try semicolon
  P.<|> P.try ifexpr
  P.<|> P.try elseif
  P.<|> P.try elseexpr
  P.<|> P.try semicolon
  P.<|> P.try block

letexpr :: P.Parsec String () Expr 
letexpr = do 
  t <- L.typeName
  ident <- L.identifier 
  L.reservedOp ":="
  Let t ident <$> expression

reassign :: P.Parsec String () Expr 
reassign = do
  ident <- L.identifier 
  L.reservedOp "<-"
  Reassign ident <$> expression

subs :: P.Parsec String () Expr 
subs = Subs <$> L.identifier 

deffn :: P.Parsec String () Expr 
deffn = do
    L.reserved "def"
    ident <- L.identifier
    params <- L.parens $ P.sepBy L.param (L.reservedOp ",")
    L.reservedOp ":"
    returnType <- L.typeName
    L.reservedOp ":="
    DefFn ident params returnType <$> expression

call :: P.Parsec String () Expr 
call = do
    ident <- L.identifier
    params <- L.parens $ P.sepBy expression (L.reservedOp ",")
    return $ Call ident params

ifexpr :: P.Parsec String () Expr 
ifexpr = do
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    If cond <$> expression 

elseif :: P.Parsec String () Expr 
elseif = do
    L.reserved "else"
    L.reserved "if"
    cond <- bExpression
    L.reservedOp "=>"
    ElseIf cond <$> expression

elseexpr :: P.Parsec String () Expr 
elseexpr = do
    L.reserved "else"
    L.reservedOp "=>"   
    Else <$> expression 

block :: P.Parsec String () Expr 
block = Block <$> (L.braces $ P.many1 expression) 
