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
import Suihei.Text.Ancillary.Monad (SuiheiParser)

import Text.ParserCombinators.Parsec

parseEffect :: String                -- ^ Function name
           -> (Expression -> Effect) -- ^ Effect constructor
           -> SuiheiParser Effect
parseEffect fun eff = do
    reserved fun
    exp <- parens parseExpression <|> parseVariable
    return (eff exp)

parseEffectIn :: String                        -- ^ Function name
              -> (Key -> Expression -> Effect) -- ^ Effect constructor
              -> SuiheiParser Effect
parseEffectIn fun eff = do
    reserved fun
    exp  <- parens parseExpression <|> parseVariable
    reserved "in"
    key  <- choice [parens parseKey, parseKey] <* spaces
    return (eff key exp)

parseEffectInAs :: String                                -- ^ Function name
                -> (Key -> Expression -> File -> Effect) -- ^ Effect constructor
                -> SuiheiParser Effect
parseEffectInAs fun eff = do
    reserved fun
    exp  <- parens parseExpression <|> parseVariable
    reserved "in"
    key  <- choice [parens parseKey, parseKey] <* spaces
    file <- optionMaybe (reserved "as" >> many1 alphaNum)
    return (eff key exp file)

parseOut :: SuiheiParser Statement
parseOut = Eff <$> parseEffect "out" Out

parseOutf :: SuiheiParser Statement
parseOutf = Eff <$> parseEffectIn "out" Outf

parseLily :: SuiheiParser Statement
parseLily = Eff <$> parseEffectInAs "lily" Lily

parseAnno :: SuiheiParser Statement
parseAnno = Eff <$> parseEffectInAs "anno" Anno