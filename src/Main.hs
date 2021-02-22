module Main where

import Text.Parsec

import Control.Monad

type Identifier = String
type Type       = String
type Param      = (Type,Identifier)

data Expr 
  = Let Type Identifier Expr
  | Subs Identifier
  | DefFn Identifier [Param] Type Expr
  | Call Identifier [Expr]
  | If Expr Expr
  | ElseIf Expr Expr
  | Else Expr
  | Semicolon
  | Block [Expr]
  deriving Show 

keywords :: [String]
keywords = ["def"]

parseProgram :: Parsec String () [Expr]
parseProgram = do 
  spaces
  result <- sepBy parseExpr spaces
  eof 
  return result

parseExpr :: Parsec String () Expr
parseExpr 
  = try parseLet
  <|> try parseSubs
  <|> try parseDefFn
  <|> try parseSemicolon

parseIdentifier :: Parsec String () Identifier
parseIdentifier = do
  head <- lower
  tail <- many (alphaNum <|> char '_' <?> "a valid character for a variable name")
  let word = head:tail
  guard (word `notElem` keywords)
  return $ head:tail

parseType :: Parsec String () Type
parseType = do
  head <- upper 
  tail <- many alphaNum 
  return $ head:tail

parseParam :: Parsec String () Param
parseParam = do
  paramType <- parseType
  spaces
  paramIdent <- parseIdentifier
  return (paramType,paramIdent)

parseSemicolon :: Parsec String () Expr
parseSemicolon = 
  do
    char ';'
    return Semicolon

parseLet :: Parsec String () Expr
parseLet = do
  varType <- parseType
  spaces
  varId   <- parseIdentifier
  spaces
  string ":="
  spaces
  Let varType varId <$> parseExpr

parseSubs :: Parsec String () Expr
parseSubs = Subs <$> parseIdentifier

parseDefFn :: Parsec String () Expr
parseDefFn = do
  string "def"
  spaces 
  fnIdent <- parseIdentifier
  spaces 
  char '('
  spaces
  paramList <- sepBy parseParam (char ',' >> spaces)
  string "):"
  spaces 
  fnReturnType <- parseType
  spaces 
  string ":="
  spaces 
  DefFn fnIdent paramList fnReturnType <$> parseExpr

parseCall :: Parsec String () Expr
parseCall = do
  ident <- parseIdentifier
  spaces
  char '('
  spaces
  args <- sepBy parseExpr (char ',' >> spaces)
  char ')'
  return $ Call ident args

parseBlock :: Parsec String () Expr
parseBlock = do
  char '{'
  spaces 
  exprs <- sepBy1 parseExpr spaces
  spaces 
  char '}'
  return $ Block exprs
  
main :: IO ()
main = do
  putStrLn "hello world"
