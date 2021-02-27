module Parser where

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as Ex

import Control.Monad

import Lexer
import Syntax

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))
unary  s f = Ex.Prefix (reservedOp s >> return (UnaryOp f))

-- arithmetic precedence
aOpTable = [[unary  "-" Not                                                                                ]
          ,[binary "*" Mul Ex.AssocLeft, binary "/" Divide Ex.AssocLeft, binary "//" IntDivide Ex.AssocLeft]
          ,[binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft                                    ]]

-- boolean precedence
bOpTable = [[unary  "not" Not             ]
           ,[binary "and" And Ex.AssocLeft]
           ,[binary "or" Or Ex.AssocLeft  ]]

-- Literal parsing

int :: P.Parsec String () Expr
int = do
  LitInt <$> integer 

float :: P.Parsec String () Expr 
float = do
  LitFloat <$> Lexer.float

string :: P.Parsec String () Expr 
string = do
  P.char '\"'
  contents <- Lexer.string
  P.char '\"'
  return $ LitString contents

bool :: P.Parsec String () Expr
bool =  (Lexer.reserved "true"  >> return (LitBool True )) 
  P.<|> (Lexer.reserved "false" >> return (LitBool False))


char :: P.Parsec String () Expr 
char = do
  P.char '\''
  contents <- Lexer.char 
  P.char '\''
  return $ LitChar contents

parseProgram :: P.Parsec String () [Expr]
parseProgram = do
  P.spaces
  result <- P.sepBy parseExpr P.spaces
  P.eof
  return result

parseExpr :: P.Parsec String () Expr
parseExpr
  = P.try parseLet
  P.<|> P.try parseSubs
  P.<|> P.try parseDefFn
  P.<|> P.try parseSemicolon

parseIdentifier :: P.Parsec String () Identifier
parseIdentifier = do
  head <- P.lower
  tail <- P.many (P.alphaNum P.<|> P.char '_' P.<?> "a valid character for a variable name")
  let word = head:tail
  return $ head:tail

parseType :: P.Parsec String () Type
parseType = do
  head <- P.upper
  tail <- P.many P.alphaNum
  return $ head:tail

parseParam :: P.Parsec String () Param
parseParam = do
  paramType <- parseType
  P.spaces
  paramIdent <- parseIdentifier
  return (paramType,paramIdent)

parseSemicolon :: P.Parsec String () Expr
parseSemicolon =
  do
    P.char ';'
    return Semicolon

parseLet :: P.Parsec String () Expr
parseLet = do
  varType <- parseType
  P.spaces
  varId   <- parseIdentifier
  P.spaces
  P.string ":="
  P.spaces
  Let varType varId <$> parseExpr

parseSubs :: P.Parsec String () Expr
parseSubs = Subs <$> parseIdentifier

parseDefFn :: P.Parsec String () Expr
parseDefFn = do
  P.string "def"
  P.spaces
  fnIdent <- parseIdentifier
  P.spaces
  P.char '('
  P.spaces
  paramList <- P.sepBy parseParam (P.char ',' >> P.spaces)
  P.string "):"
  P.spaces
  fnReturnType <- parseType
  P.spaces
  P.string ":="
  P.spaces
  DefFn fnIdent paramList fnReturnType <$> parseExpr

parseCall :: P.Parsec String () Expr
parseCall = do
  ident <- parseIdentifier
  P.spaces
  P.char '('
  P.spaces
  args <- P.sepBy parseExpr (P.char ',' >> P.spaces)
  P.char ')'
  return $ Call ident args

parseBlock :: P.Parsec String () Expr
parseBlock = do
  P.char '{'
  P.spaces
  exprs <- P.sepBy1 parseExpr P.spaces
  P.spaces
  P.char '}'
  return $ Block exprs
