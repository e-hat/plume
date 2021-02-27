module Parser 
(
    parseExpr
) where

import qualified Text.Parsec as P

import Control.Monad

import Lexer
import Syntax

keywords :: [String]
keywords = ["def"]

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
  guard (word `notElem` keywords)
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
