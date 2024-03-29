{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Parsing.Lexer where

import Text.Parsec as P
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok

languageDef =
    emptyDef
        { Tok.commentStart = "/*"
        , Tok.commentEnd = "*/"
        , Tok.commentLine = "#"
        , Tok.identStart = P.lower
        , Tok.identLetter = P.alphaNum
        , Tok.reservedNames =
            [ "if"
            , "for"
            , "while"
            , "def"
            , "true"
            , "false"
            , "else"
            , "not"
            , "and"
            , "or"
            , "return"
            , "main"
            , "yield"
            ]
        , Tok.reservedOpNames =
            [ "+"
            , "-"
            , "/"
            , ":="
            , "<"
            , "<="
            , "="
            , "!="
            , ">"
            , "and"
            , "or"
            , "not"
            , "<-"
            , ":"
            , ","
            , "=>"
            ]
        }

lexer = Tok.makeTokenParser languageDef

identifier = Tok.identifier lexer

reserved = Tok.reserved lexer

reservedOp = Tok.reservedOp lexer

parens = Tok.parens lexer

braces = Tok.braces lexer

semicolon = Tok.semi lexer

whiteSpace = Tok.whiteSpace lexer

natOrFloat = Tok.naturalOrFloat lexer

char = Tok.charLiteral lexer

string = Tok.stringLiteral lexer

typeName = Tok.lexeme lexer ((:) <$> P.upper <*> P.many P.alphaNum)

param = Tok.lexeme lexer ((,) <$> typeName <*> identifier)
