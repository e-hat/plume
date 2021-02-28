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
    P.<|> subs
    P.<|> int
    P.<|> float
    P.<|> call

--------------------------------------------------------
--------------- boolean expressions---------------------
--------------------------------------------------------
bOpTable = [[unary  "not" Not             ]
           ,[binary "and" And Ex.AssocLeft]
           ,[binary "or"  Or  Ex.AssocLeft]]

bExpression :: P.Parsec String () Expr
bExpression = Ex.buildExpressionParser bOpTable bTerm

bTerm =   L.parens bExpression
    P.<|> subs
    P.<|> bool
    P.<|> call
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

-- types MUST start with capital in Plume
typeName :: P.Parsec String () Type
typeName = (:) <$> P.upper <*> P.many P.alphaNum

letexpr :: P.Parsec String () Expr 
letexpr = do 
  t <- typeName
  ident <- L.identifier 
  L.reservedOp ":="
  Let t ident <$> expression

expression :: P.Parsec String () Expr 
expression = undefined

subs :: P.Parsec String () Expr 
subs = undefined 

call :: P.Parsec String () Expr 
call = undefined
