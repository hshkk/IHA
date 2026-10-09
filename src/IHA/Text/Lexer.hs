module IHA.Text.Lexer (
    lexer,
    lexeme, ident, symbol, int, nat,
    parens,
    reserved, reservedOp,
    whitespace
) where

import IHA.Text.Ancillary.Monad (Parser)

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
lexer = iha

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

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

iha :: TokenParser st
iha = makeTokenParser ihaDef

ihaDef :: LanguageDef st
ihaDef = emptyDef
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = True
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf "_'"
                , reservedOpNames = [">", "^", ">|", "|<"]
                , reservedNames   = [ "infer"
                                    , "out", "in", "as"
                                    , "lily", "anno"
                                    , "C", "D", "E", "F", "G", "A", "B"
                                    , "i", "ii", "iii", "iv", "v", "vi", "vii"
                                    , "I", "II", "III", "IV", "V", "VI", "VII"
                                    , "w", "h", "q", "e", "s", "t" ]
                , caseSensitive   = True
                }