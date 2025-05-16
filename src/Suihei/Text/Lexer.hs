module Suihei.Text.Lexer (
    lexer,
    lexeme, ident, symbol, int, nat,
    parens,
    reserved, reservedOp
) where

import Suihei.Text.Transformation (Suihei)

import GHC.Natural (Natural)

import Text.Parsec (alphaNum, letter, oneOf, (<|>))
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

lexeme :: Suihei a -> Suihei a
lexeme = Token.lexeme lexer

ident :: Suihei String
ident = Token.identifier lexer

symbol :: String -> Suihei String
symbol = Token.symbol lexer

int :: Suihei Int
int = (fromIntegral :: Integer -> Int) <$> Token.integer lexer

nat :: Suihei Natural
nat = (fromIntegral :: Integer -> Natural) <$> Token.integer lexer

parens :: Suihei a -> Suihei a
parens = Token.parens lexer

reserved :: String -> Suihei ()
reserved = Token.reserved lexer

reservedOp :: String -> Suihei ()
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
                , reservedOpNames = [">", "^", ">|", "|<"]
                , reservedNames   = [ "infer"
                                    , "out", "in"
                                    , "midi" ]
                , caseSensitive   = True
                }