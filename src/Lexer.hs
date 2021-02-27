module Lexer
(
    languageDef,
    lexer,
    identifier,
    reserved,
    reservedOp,
    parens,
    semicolon,
    whiteSpace
) where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
import Text.Parsec as P

languageDef = 
    emptyDef { Tok.commentStart  = "/*"
             , Tok.commentEnd    = "*/"
             , Tok.commentLine   = "#"
             , Tok.identStart    = P.letter
             , Tok.identLetter   = P.alphaNum
             , Tok.reservedNames = [ "if"
                                  , "for"
                                  , "while"
                                  , "def"
                                  , "true"
                                  , "false"
                                  , "else"
                                  , "else"
                                  , "not"
                                  , "and"
                                  , "or"
                                  ] 
              , Tok.reservedOpNames  = [ "+", "-", "/", "//", ":="
                                  , "<", "<=", "=", "and", "or", "not"
                                  ]
              }

lexer = Tok.makeTokenParser languageDef

identifier = Tok.identifier lexer
reserved   = Tok.reserved   lexer
reservedOp = Tok.reservedOp lexer
parens     = Tok.parens     lexer
semicolon  = Tok.semi       lexer
whiteSpace = Tok.whiteSpace lexer
