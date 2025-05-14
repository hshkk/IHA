module Suihei.Text.Lexer (
    lexer,
    lexeme, ident, symbol, int, nat,
    parens,
    reserved, reservedOp
) where

import GHC.Natural (Natural)

import Text.Parsec (alphaNum, letter, oneOf, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Token
    (GenLanguageDef (caseSensitive, 
                     commentStart, commentEnd,
                     commentLine, nestedComments, 
                     identStart, identLetter, 
                     reservedOpNames, reservedNames),
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

int :: Parser Int
int = (fromIntegral :: Integer -> Int) <$> Token.integer lexer

nat :: Parser Natural
nat = (fromIntegral :: Integer -> Natural) <$> Token.integer lexer

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