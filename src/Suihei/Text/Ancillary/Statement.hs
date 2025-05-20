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
    parseOutf,
    parseLily,
    parseAnno)
import Suihei.Text.Ancillary.Expression (parseExpression)
import Suihei.Text.Ancillary.Monad (Parser)

import Text.ParserCombinators.Parsec (try, choice)

parseStatement :: Parser Statement
parseStatement = parseEffect ?<|> parseDefinition
    where parseEffect = choice [try parseOutf, parseOut, parseLily, parseAnno]
          parseDefinition = ident           >>= \var ->
                            reservedOp "="  >>
                            parseExpression >>= \exp ->
                            return (Def var exp)