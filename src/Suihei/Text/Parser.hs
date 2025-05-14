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
import Suihei.Data.Composition (pattern Unit)
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Lexer

import Text.Parsec.Expr (
    OperatorTable, Operator (Infix), Assoc (AssocLeft, AssocRight),
    buildExpressionParser)
import Text.ParserCombinators.Parsec

import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Ratio ((%))
import GHC.Natural (Natural)

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable (choice [parseTerm])

operatorTable :: OperatorTable String () Identity Expression
operatorTable = [[ifxl (parseOperator ">|" (:>|))
                , ifxl (parseOperator "|<" (:|<))
                , ifxl (parseOperator ">"  (:>.))
                , ifxl (parseOperator "^"  (:^.))]]
    where ifxl x = Infix x AssocLeft
          ifxr x = Infix x AssocRight

parseOperator :: String -> (Expression -> Expression -> Expression) -> Parser (Expression -> Expression -> Expression)
parseOperator s op = op <$ reservedOp s

parseTerm :: Parser Expression
parseTerm = choice [parseInfer, parseValue]

parseInfer :: Parser Expression
parseInfer = do
    reserved "infer"
    e <- choice [parens parseExpression, parseTerm]
    return (Infer e)

parseValue :: Parser Expression
parseValue = try parseLiteral <|> parseVariable

parseLiteral :: Parser Expression
parseLiteral = choice [parseNote]

parseNote :: Parser Expression
parseNote = do
    m <- oneOf notes
    a <- parseAccidentals
    o <- (fromIntegral :: Integer -> Natural) <$> number
    d <- parseDuration
    let dia  = fromJust (m `elemIndex` notes)
    let note = ((toMod dia, a), o)
    (return . Value . Music . Unit) (Just note, d)
    where notes = "CDEFGAB"
          parseAccidentals = length <$> many1 (symbol "#")
                         <|> negate . length <$> many1 (symbol "b")
                         <|> pure 0

parseDuration :: Parser Duration
parseDuration = do
    num <- (fromIntegral :: Integer -> Int) <$> number
    _ <- symbol "/"
    den <- (fromIntegral :: Integer -> Int) <$> number
    return (num % den)

parseVariable :: Parser Expression
parseVariable = Variable <$> ident

parseStatement :: Parser Statement
parseStatement = do
    x <- ident
    reservedOp "="
    e <- parseExpression
    return (Def x e)

parseLn :: String -> Either String Statement
parseLn ln = case parse parseStatement ln ln of
    Left err -> Left (show err)
    Right st -> Right st