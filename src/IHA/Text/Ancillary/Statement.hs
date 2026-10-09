module IHA.Text.Ancillary.Statement (
    parseStatement
) where

import IHA.Data.Abstract (
    Statement  (..))
import IHA.Text.Lexer (
    ident,
    reservedOp)
import IHA.Text.Ancillary.Combinator ((?<|>))
import IHA.Text.Ancillary.Effect (
    parseOut,
    parseLily,
    parseAnno)
import IHA.Text.Ancillary.Expression (parseExpression)
import IHA.Text.Ancillary.Monad (Parser)

import Text.ParserCombinators.Parsec (choice)

import Prelude hiding (exp)

parseStatement :: Parser Statement
parseStatement = parseEffect ?<|> parseDefinition
    where parseEffect = choice [parseOut, parseLily, parseAnno]
          parseDefinition = ident           >>= \var ->
                            reservedOp "="  >>
                            parseExpression >>= \exp ->
                            return (Def var exp)