{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Suihei.Text.Ancillary.Expression (
    parseExpression,
    parseOperator,
    parseTerm,
    parseInfer,
    parseValue, parseLiteral,
    parseKey,
    parseVariable
) where

import Suihei.Data.Abstract (
    Expression (..),
    Value      (..))
import Suihei.Data.Chromatic (Accidental, flat, dflat)
import Suihei.Data.Modular (toMod)
import Suihei.Data.Chordal (
    Key, Mode (..),
    pattern Analysis,
    Variation (..),
    pattern Dim, pattern Aug, base)
import Suihei.Data.Notal (Octave, Note)
import Suihei.Data.Composition (pattern Unit, pattern (:^))
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Lexer
import Suihei.Text.Ancillary.Combinator (
    (?<|>), returnM,
    fromJustDuration,
    fromJustOctaviation,
    fromJustVariations)
import Suihei.Text.Ancillary.Monad (
    Parser,
    OpTable)
import Suihei.Text.Ancillary.State (
    getVariations,  modifyVariations,
    getOctaviation, modifyOctaviation,
    getDuration,    modifyDuration)

import Text.Parsec (string', parserZero)
import Text.Parsec.Expr (
    Operator (Infix), Assoc (AssocLeft),
    buildExpressionParser)
import Text.ParserCombinators.Parsec hiding (Parser)

import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (elemIndex, foldl1')
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import Data.Ratio ((%))
import Data.List.Extra (notNull)
import Prelude hiding (exp)

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable parseTerm

operatorTable :: OpTable Expression
operatorTable = [ [ ifxl (parseOperator "^"  (:^.))]
                , [ ifxl (parseOperator ">|" (:>|))
                  , ifxl (parseOperator "|<" (:|<))
                  , ifxl (parseOperator ">"  (:>.)) ] ]
    where ifxl op = Infix op AssocLeft
       -- ifxr op = Infix op AssocRight

parseOperator :: String
              -> (Expression -> Expression -> Expression)
              -> Parser (Expression -> Expression -> Expression)
parseOperator str op = op <$ reservedOp str

parseTerm :: Parser Expression
parseTerm = parseInfer
       ?<|> parseReify
       ?<|> parseValue
       ?<|> parens parseExpression

parseInfer :: Parser Expression
parseInfer = do
    reserved "infer"
    exp <- parens parseExpression <|> parseVariable
    key <- optionMaybe (reserved "in" >> choice [parens parseKey, parseKey] <* spaces)
    return (Infer exp (fromMaybe base key))

parseReify :: Parser Expression
parseReify = do
    reserved "reify"
    exp <- parens parseExpression <|> parseVariable
    key <- optionMaybe (reserved "in" >> choice [parens parseKey, parseKey] <* spaces)
    return (Reify exp (fromMaybe base key))

parseValue :: Parser Expression
parseValue = parseLiteral
        ?<|> parseVariable

parseLiteral :: Parser Expression
parseLiteral = parseChord
           <|> parseRest
          ?<|> parseAbsoluteNote
          ?<|> parseRelativeNote
          ?<|> parseAnalysis

parseChord :: Parser Expression
parseChord = do
    _     <- symbol "["
    notes <- sepBy1 parseNote spaces
    _     <- symbol "]"
    dur   <- parseDuration
    let m = Value . Music $ foldl1' (:^) (map (Unit . (, fromJustDuration dur) . Just) notes)
    return m

parseNote :: Parser Note
parseNote = parseAbsolute ?<|> parseRelative

parseAbsolute :: Parser Note
parseAbsolute = do
    name <- oneOf absolutes
    acc  <- parseAccidentals
    oct  <- parseAbsoluteNoteSugar <* spaces
       ?<|> parseOctaviationPropagation (returnM =<< nat) <* spaces
    let dia  = fromJust (name `elemIndex` absolutes)
    let note = ((toMod dia, acc), fromJustOctaviation oct)
    return note
    where parseAbsoluteNoteSugar :: Parser (Maybe Octave)
          parseAbsoluteNoteSugar = do
              oct <- nat
              modifyOctaviation (Just oct)
              returnM oct
          absolutes = "CDEFGAB"

parseRelative :: Parser Note
parseRelative = do
    name <- oneOf relatives
    acc  <- parseAccidentals
    oct  <- parseOctaviationPropagation (returnM =<< nat) <* spaces
    let dia  = fromJust (name `elemIndex` relatives)
    let note = ((toMod dia, acc), fromJustOctaviation oct)
    return note
    where relatives = "1234567"

parseAbsoluteNote :: Parser Expression
parseAbsoluteNote = do
    note <- parseAbsolute
    dur  <- parseDuration
    return (Value . Music . Unit $ (Just note, fromJustDuration dur))

parseRelativeNote :: Parser Expression
parseRelativeNote = do
    note <- parseRelative
    dur  <- parseDuration
    return (Value . Music . Unit $ (Just note, fromJustDuration dur))

parseRest :: Parser Expression
parseRest = do
    _ <- symbol "*"
    dur <- parseDuration
    return (Value . Music . Unit $ (Nothing, fromJustDuration dur))

parseAnalysis :: Parser Expression
parseAnalysis = parseShift ?<|> parseSymbol

parseShift :: Parser Expression
parseShift = do
    key <- parseKey <* spaces
    reservedOp ":"  <* spaces
    ana <- try parseAnalysis <|> parseVariable
    return (PartialShift key ana)

parseSymbol :: Parser Expression
parseSymbol = do
    st    <- getVariations
    key   <- parseKey
    vars1 <- parseVariationSugar <* spaces
    vars2 <- parseVariations     <* spaces
    -- If no propagation and no list of modifications is present, the parser defaults to no modifications.
    vars' <- if isNothing st && isNothing vars2 then returnM [] else return vars2
    dur   <- parseDuration
    let analysis = Analysis key (vars1 ++ fromJustVariations vars')
    (return . Value . Progression . Unit) (analysis, fromJustDuration dur)

parseVariable :: Parser Expression
parseVariable = Variable <$> ident

parsePropagation :: Parser (Maybe a)       -- ^ Retreive from state
                 -> (Maybe a -> Parser ()) -- ^ Transform the state
                 -> Parser (Maybe a)       -- ^ Parser
                 -> Parser (Maybe a)
parsePropagation get modify parser = do
    st  <- get
    option st (parseLimits ?<|> parseTermination st)
    where parseTermination st = do
              _   <- symbol "|"
              modify Nothing
              return st
          parseLimits = do
              _   <- symbol "|"
              res <- parser
              sym <- optionMaybe (symbol "|")
              unless (isJust sym) (modify res)
              return res

parseOctaviationPropagation :: Parser (Maybe Octave) -> Parser (Maybe Octave)
parseOctaviationPropagation = parsePropagation getOctaviation modifyOctaviation

parseVariationPropagation :: Parser (Maybe [Variation]) -> Parser (Maybe [Variation])
parseVariationPropagation = parsePropagation getVariations modifyVariations

parseAccidentals :: Parser Accidental
parseAccidentals = choice [ length          <$> many1 (symbol "#")
                          , negate . length <$> many1 (symbol "b")
                          , return 0]

parseKey :: Parser Key
parseKey = do
    acc  <- parseAccidentals
    name <- choice (map string' analyses)
    let dia = fromJust (map toLower name `elemIndex` ordered)
    let key = ((toMod dia, acc), if map toLower name /= name then Maj else Min)
    return key
    where analyses = [ "vii", "vi", "v", "iv", "iii", "ii", "i"
                     , "VII", "VI", "V", "IV", "III", "II", "I" ]
          ordered  = reverse (take 7 analyses)

parseVariationSugar :: Parser [Variation]
parseVariationSugar = do
    dim <- option [] ((char 'o' >> return [Dim]) 
                  <|> (char '%' >> return [Dim, Adj 7 flat]))
    aug <- option []  (char '+' >> return [Aug])
    sv  <- option []  (char '7' >> return [Adj 7 flat, Add 7])
    sn  <- option []  (char '/' >> parseKey >>= \key -> return [Snd key])
    if notNull sv 
    then
        case dim of
            [Dim]          -> return (dim ++ [Adj 7 dflat, Add 7] ++ aug ++ sn)
            [Dim, Adj 7 _] -> return (dim ++ [Add 7] ++ aug ++ sn)
            _              -> return (dim ++ aug ++ sv ++ sn)
    else 
        case dim of
            [Dim, Adj 7 _] -> error "Text.Ancillary.Expression: Half-diminished without seventh"
            _ -> return (dim ++ aug ++ sv ++ sn)

parseVariations :: Parser (Maybe [Variation])
parseVariations = option (Just []) (parseVariationPropagation (Just <$> sepBy1 parseVariation (symbol ",")))
    where parseVariation :: Parser Variation
          parseVariation = choice [ char 'o'     >> return Dim
                                  , char '+'     >> return Aug
                                  , char '/'     >> parseKey >>= \key -> return (Snd key)
                                  , char 'i'     >> int      >>= \n   -> return (Inv n)
                                  , char 'a'     >> int      >>= \n   -> return (Add n)
                                  , string' "no" >> int      >>= \n   -> return (Sub n) ]

parseDuration :: Parser (Maybe Duration)
parseDuration = do
    dur <- getDuration
    res <- option Nothing (parseDurationLiteral
                      <|> (parseDurationVariable >>= \dur1  ->
                           parseDots dur1        >>= \dur2 ->
                           returnM (fromJustDuration dur1
                                  + fromJustDuration dur2))) <* spaces
    case res of
        -- Returns the sealed duration with no state alteration.
        Nothing -> return dur
        -- Otherwise, updates the active propagated duration.
        Just _  -> modifyDuration res >> return res
    where parseDurationVariable :: Parser (Maybe Duration)
          parseDurationVariable = choice [ symbol "w" >> returnM (1 % 1)
                                         , symbol "h" >> returnM (1 % 2)
                                         , symbol "q" >> returnM (1 % 4)
                                         , symbol "e" >> returnM (1 % 8)
                                         , symbol "s" >> returnM (1 % 16)
                                         , symbol "t" >> returnM (1 % 32)]
          parseDots :: Maybe Duration -> Parser (Maybe Duration)
          parseDots dur' = do
              dots <- many (char '.')
              case dur' of
                  Nothing  -> parserZero
                  Just dur -> if null dots
                             then returnM 0
                             else returnM (dur * sum [1 % (2 * i) | i <- [1..length dots]])
          parseDurationLiteral :: Parser (Maybe Duration)
          parseDurationLiteral = int        >>= \num ->
                                 symbol "/" >>
                                 int        >>= \den ->
                                 returnM (num % den)