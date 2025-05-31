module Suihei.Text.Ancillary.Statement (
    parseStatement
) where

import Suihei.Data.Abstract (
    Statement  (..))
import Suihei.Text.Lexer (
    ident,
    reservedOp)
import Suihei.Text.Ancillary.Combinator ((?<|>))
import Suihei.Text.Ancillary.Effect (
    parseOut,
    parseLily,
    parseAnno)
import Suihei.Text.Ancillary.Expression (parseExpression)
import Suihei.Text.Ancillary.Monad (Parser)

import Text.ParserCombinators.Parsec (choice)

import Prelude hiding (exp)

parseStatement :: Parser Statement
parseStatement = parseEffect ?<|> parseDefinition
    where parseEffect = choice [parseOut, parseLily, parseAnno]
          parseDefinition = ident           >>= \var ->
                            reservedOp "="  >>
                            parseExpression >>= \exp ->
                            return (Def var exp)