{-# LANGUAGE PatternSynonyms #-}

module Suihei.Text.Parser (
    parseLn
) where

import Suihei.Data.Abstract (
    Program,
    Statement  (..),
    Environment, Binding,
    Expression (..),
    Value      (..),
    Variable,
    Effect     (..))
import Suihei.Data.Modular (toMod)
import Suihei.Data.Chordal (Mode (..), pattern Analysis, Variation(..), Key)
import Suihei.Data.Composition (pattern Unit)
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Lexer

import Text.Parsec (string', ParsecT)
import Text.Parsec.Expr (
    OperatorTable, Operator (Infix), Assoc (AssocLeft, AssocRight),
    buildExpressionParser)
import Text.ParserCombinators.Parsec

import Data.Char (toLower)
import Data.Functor.Identity (Identity)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ratio ((%))

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable parseTerm

operatorTable :: OperatorTable String () Identity Expression
operatorTable = [ [ifxl (parseOperator "^" (:^.))]
                , [ ifxl (parseOperator ">|" (:>|))
                  , ifxl (parseOperator "|<" (:|<))
                  , ifxl (parseOperator ">" (:>.)) ] ]
    where ifxl op = Infix op AssocLeft
          ifxr op = Infix op AssocRight

parseOperator :: String -> (Expression -> Expression -> Expression) -> Parser (Expression -> Expression -> Expression)
parseOperator s op = op <$ reservedOp s

parseTerm :: Parser Expression
parseTerm = try parseInfer <|> parseValue <|> parens parseExpression

parseInfer :: Parser Expression
parseInfer = do
    reserved "infer"
    e <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    return (Infer e)

parseValue :: Parser Expression
parseValue = try parseLiteral <|> parseVariable

parseLiteral :: Parser Expression
parseLiteral = try parseAbsoluteNote 
           <|> try parseRelativeNote 
           <|> try parseRest
           <|> parseAnalysis

parseAccidentals :: ParsecT String () Identity Int
parseAccidentals = choice [ length <$> many1 (symbol "#")
                          , negate . length <$> many1 (symbol "b")
                          , pure 0]

parseAbsoluteNote :: Parser Expression
parseAbsoluteNote = do
    m <- oneOf notes
    a <- parseAccidentals
    o <- nat
    d <- parseDuration
    let dia  = fromJust (m `elemIndex` notes)
    let note = ((toMod dia, a), o)
    (return . Value . Music . Unit) (Just note, d)
    where notes = "CDEFGAB"

parseRelativeNote :: Parser Expression
parseRelativeNote = do
    m <- oneOf notes
    a <- parseAccidentals
    o <- between (symbol "|") (symbol "|") nat
    d <- parseDuration
    let dia = fromJust (m `elemIndex` notes)
    let note = ((toMod dia, a), o)
    (return . Value . Music . Unit) (Just note, d)
    where notes = "1234567"

parseRest :: Parser Expression
parseRest = do
    _ <- symbol "*"
    d <- parseDuration
    (return . Value . Music . Unit) (Nothing, d)

parseKey :: Parser Key
parseKey = do
    a <- parseAccidentals
    m  <- choice (map string' analyses)
    let dia = fromJust (map toLower m `elemIndex` reverse (take 7 analyses))
    let key = ((toMod dia, a), if map toLower m /= m then Maj else Min)
    return key
    where analyses = [ "vii", "vi", "v", "iv", "iii", "ii", "i"
                     , "VII", "VI", "V", "IV", "III", "II", "I" ]

parseAnalysis :: Parser Expression
parseAnalysis = do
    key <- parseKey
    ss  <- parseVariationSugar       <* spaces
    vs  <- option [] parseVariations <* spaces
    d   <- parseDuration
    let analysis = Analysis key (ss ++ vs)
    (return . Value . Progression . Unit) (analysis, d)

parseVariationSugar :: Parser [Variation]
parseVariationSugar = do
    dim <- option [] (char 'o' >> return [Dim])
    aug <- option [] (char '+' >> return [Aug])
    snd <- option [] (char '/' >> parseKey >>= \key -> return [Snd key])
    return (dim ++ aug ++ snd)

parseVariations :: Parser [Variation]
parseVariations = do
    between (symbol "|") (symbol "|") (sepBy parseVariation (symbol ","))
    where parseVariation :: Parser Variation
          parseVariation = choice [ char 'o' >> return Dim
                                  , char '+' >> return Aug
                                  , char '/' >> parseKey >>= \key -> return (Snd key)
                                  , char 'i' >> int >>= \n -> return (Inv n)
                                  , char 'a' >> int >>= \n -> return (Add n)
                                  , string' "no" >> int >>= \n -> return (Sub n) ]

parseDuration :: Parser Duration
parseDuration = parseDurationLiteral <|> parseDurationVariable
    where parseDurationVariable = choice [ symbol "w" >> return (1 % 1)
                                         , symbol "h" >> return (1 % 2)
                                         , symbol "q" >> return (1 % 4)
                                         , symbol "e" >> return (1 % 8)
                                         , symbol "s" >> return (1 % 16)
                                         , symbol "t" >> return (1 % 32)]
          parseDurationLiteral = int >>= \num -> 
                                 char '/' >>
                                 int >>= \den -> 
                                 return (num % den)

parseVariable :: Parser Expression
parseVariable = Variable <$> ident

parseOut :: Parser Statement
parseOut = do
    reserved "out"
    e <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    return (Eff (Out e))

parseOutf :: Parser Statement
parseOutf = do
    reserved "out"
    e   <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    reserved "in"
    key <- choice [parens parseKey, parseKey] <* spaces
    return (Eff (Outf key e))

parseMidi :: Parser Statement
parseMidi = do
    reserved "midi"
    e <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    return (Eff (Midi e))

parseStatement :: Parser Statement
parseStatement = (try parseEffect <|> parseDefinition) <* eof
    where parseDefinition = ident >>= \x -> 
                            reservedOp "=" >> 
                            parseExpression >>= \e -> 
                            return (Def x e)
          parseEffect = choice [try parseOutf, parseOut, parseMidi]

parseLn :: String -> Either String Statement
parseLn ln = case parse parseStatement ln ln of
    Left err -> Left (show err)
    Right st -> Right st