module Suihei.Text.Ancillary.Effect (
    parseOut, parseOutf, parseLily, parseAnno
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Expression (..),
    Effect     (..), File)
import Suihei.Data.Chordal (
    Key)
import Suihei.Text.Lexer
import Suihei.Text.Ancillary.Expression (
    parseExpression,
    parseKey,
    parseVariable)
import Suihei.Text.Ancillary.Monad (Parser)

import Text.ParserCombinators.Parsec hiding (Parser)

parseEffect :: String                -- ^ Function name
           -> (Expression -> Effect) -- ^ Effect constructor
           -> Parser Effect
parseEffect fun eff = do
    reserved fun
    exp <- parens parseExpression <|> parseVariable
    return (eff exp)

parseEffectIn :: String                        -- ^ Function name
              -> (Expression -> Key -> Effect) -- ^ Effect constructor
              -> Parser Effect
parseEffectIn fun eff = do
    reserved fun
    exp  <- parens parseExpression <|> parseVariable
    reserved "in"
    key  <- choice [parens parseKey, parseKey] <* spaces
    return (eff exp key)

parseEffectInAs :: String                                -- ^ Function name
                -> (Expression -> Key -> File -> Effect) -- ^ Effect constructor
                -> Parser Effect
parseEffectInAs fun eff = do
    reserved fun
    exp  <- parens parseExpression <|> parseVariable
    reserved "in"
    key  <- choice [parens parseKey, parseKey] <* spaces
    file <- optionMaybe (reserved "as" >> many1 alphaNum)
    return (eff exp key file)

parseOut :: Parser Statement
parseOut = Eff <$> parseEffect "out" Out

parseOutf :: Parser Statement
parseOutf = Eff <$> parseEffectIn "out" Outf

parseLily :: Parser Statement
parseLily = Eff <$> parseEffectInAs "lily" Lily

parseAnno :: Parser Statement
parseAnno = Eff <$> parseEffectInAs "anno" Anno