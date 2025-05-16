module Suihei.Text.Ancillary.Effect (
    parseEffectAbs, parseEffectCon,
    parseOut, parseOutf, parseMidi
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Expression (..),
    Effect     (..))
import Suihei.Data.Chordal (
    Key)
import Suihei.Text.Lexer
import Suihei.Text.Ancillary.Expression (
    parseExpression, 
    parseKey, 
    parseVariable)
import Suihei.Text.Ancillary.Monad (SuiheiParser)

import Text.ParserCombinators.Parsec


parseEffectAbs :: String                 -- ^ Function name
               -> (Expression -> Effect) -- ^ Effect constructor
               -> SuiheiParser Statement
parseEffectAbs fun eff = do
    reserved fun
    exp <- parens parseExpression <|> parseVariable
    return (Eff . eff $ exp)

parseEffectCon :: String                        -- ^ Function name
              -> (Key -> Expression -> Effect) -- ^ Effect constructor
              -> SuiheiParser Statement
parseEffectCon fun eff = do
    reserved fun
    exp <- parens parseExpression <|> parseVariable
    reserved "in"
    key <- choice [parens parseKey, parseKey] <* spaces
    return (Eff . eff key $ exp)

parseOut :: SuiheiParser Statement
parseOut = parseEffectAbs "out" Out

parseOutf :: SuiheiParser Statement
parseOutf = parseEffectCon "out" Outf

parseMidi :: SuiheiParser Statement
parseMidi = parseEffectCon "midi" Midi