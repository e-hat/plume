module Lexer where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
import Text.Parsec as P

languageDef = 
    emptyDef { Tok.commentStart  = "/*"
             , Tok.commentEnd    = "*/"
             , Tok.commentLine   = "#"
             , Tok.identStart    = P.lower
             , Tok.identLetter   = P.alphaNum
             , Tok.reservedNames = [ "if"
                                  , "for"
                                  , "while"
                                  , "def"
                                  , "true"
                                  , "false"
                                  , "else"
                                  , "not"
                                  , "and"
                                  , "or"
                                  ] 
              , Tok.reservedOpNames  = [ "+", "-", "/", "//", ":="
                                  , "<", "<=", "=", "!=", ">", "and", "or", "not"
                                  , "<-", ":", ",", "=>"
                                  ]
              }

lexer = Tok.makeTokenParser languageDef

identifier = Tok.identifier    lexer
reserved   = Tok.reserved      lexer
reservedOp = Tok.reservedOp    lexer
parens     = Tok.parens        lexer
braces     = Tok.braces        lexer
semicolon  = Tok.semi          lexer
whiteSpace = Tok.whiteSpace    lexer
integer    = Tok.integer       lexer
float      = Tok.float         lexer
char       = Tok.charLiteral   lexer
string     = Tok.stringLiteral lexer
typeName   = Tok.lexeme        lexer ((:) <$> P.upper  <*> P.many P.alphaNum)
param      = Tok.lexeme        lexer ((,) <$> typeName <*> identifier       )
