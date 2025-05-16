module Suihei.Text.Lexer (
    lexer,
    lexeme, ident, symbol, int, nat,
    parens,
    reserved, reservedOp
) where

import Suihei.Text.Ancillary.Monad (SuiheiParser)

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

lexeme :: SuiheiParser a -> SuiheiParser a
lexeme = Token.lexeme lexer

ident :: SuiheiParser String
ident = Token.identifier lexer

symbol :: String -> SuiheiParser String
symbol = Token.symbol lexer

int :: SuiheiParser Int
int = (fromIntegral :: Integer -> Int) <$> Token.integer lexer

nat :: SuiheiParser Natural
nat = (fromIntegral :: Integer -> Natural) <$> Token.integer lexer

parens :: SuiheiParser a -> SuiheiParser a
parens = Token.parens lexer

reserved :: String -> SuiheiParser ()
reserved = Token.reserved lexer

reservedOp :: String -> SuiheiParser ()
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