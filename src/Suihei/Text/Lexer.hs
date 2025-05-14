module Suihei.Text.Lexer (
    lexer,
    lexeme, ident, symbol, number,
    parens,
    reserved, reservedOp
) where

import Text.Parsec (alphaNum, letter, oneOf, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Token
    (GenLanguageDef (caseSensitive, commentStart, commentEnd,
                     commentLine, nestedComments, identStart, identLetter, opStart,
                     opLetter, reservedOpNames, reservedNames),
     LanguageDef, TokenParser, makeTokenParser)
import Text.ParserCombinators.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer :: TokenParser st
lexer = suihei

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

ident :: Parser String
ident = Token.identifier lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

number :: Parser Integer
number = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

suihei :: TokenParser st
suihei = makeTokenParser suiheiDef

suiheiDef :: LanguageDef st
suiheiDef = emptyDef
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = True
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf "_'"
                , reservedOpNames = [">", "^", ">|", "|<", "|"]
                , reservedNames   = ["infer", "out", "outf", "midi"]
                , caseSensitive   = True
                }