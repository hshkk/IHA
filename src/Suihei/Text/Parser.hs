{-# LANGUAGE PatternSynonyms #-}

module Suihei.Text.Parser (
    parseLn
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Expression (..),
    Value      (..),
    Effect     (..))
import Suihei.Data.Modular (toMod)
import Suihei.Data.Chordal (
    Key, Mode (..),
    Analysis  (..), pattern Analysis, TAnalysis,
    Variation (..))
import Suihei.Data.Notal (Octave)
import Suihei.Data.Composition (pattern Unit)
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Lexer
import Suihei.Text.Transformation

import Text.Parsec (string', parserZero)
import Text.Parsec.Expr (
    Operator (Infix), Assoc (AssocLeft, AssocRight),
    buildExpressionParser)
import Text.Parsec.Prim (runParserT)
import Text.ParserCombinators.Parsec

import Data.Char (toLower)
import Data.Functor.Identity (Identity (Identity))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Ratio ((%))

parseExpression :: Suihei Expression
parseExpression = buildExpressionParser operatorTable parseTerm

operatorTable :: SuiheiOperatorTable Expression
operatorTable = [ [ ifxl (parseOperator "^"  (:^.))]
                , [ ifxl (parseOperator ">|" (:>|))
                  , ifxl (parseOperator "|<" (:|<))
                  , ifxl (parseOperator ">"  (:>.)) ] ]
    where ifxl op = Infix op AssocLeft
          ifxr op = Infix op AssocRight

parseOperator :: String -> (Expression -> Expression -> Expression) -> Suihei (Expression -> Expression -> Expression)
parseOperator s op = op <$ reservedOp s

parseTerm :: Suihei Expression
parseTerm = try parseInfer
        <|> parseValue
        <|> parens parseExpression

parseInfer :: Suihei Expression
parseInfer = do
    reserved "infer"
    exp <- choice [parens parseExpression, parseVariable]
    return (Infer exp)

parseValue :: Suihei Expression
parseValue = try parseLiteral
         <|> parseVariable

parseLiteral :: Suihei Expression
parseLiteral = try parseAbsoluteNote
           <|> try parseRelativeNote
           <|> try parseRest
           <|> parseAnalysis

parseAccidentals :: Suihei Int
parseAccidentals = choice [ length <$> many1 (symbol "#")
                          , negate . length <$> many1 (symbol "b")
                          , pure 0]

parseAbsoluteNote :: Suihei Expression
parseAbsoluteNote = do
    name <- oneOf notes
    acc  <- parseAccidentals
    oct  <- try (nat >>= \oct -> 
            modifyOctaviation (Just oct) >> 
            returnM oct)     <* spaces
        <|> parseOctaviation <* spaces
    dur  <- parseDuration
    let dia  = fromJust (name `elemIndex` notes)
    let note = ((toMod dia, acc), fromJust oct)
    return (Value . Music . Unit $ (Just note, fromJust dur))
    where notes = "CDEFGAB"

parseRelativeNote :: Suihei Expression
parseRelativeNote = do
    name <- oneOf notes
    acc  <- parseAccidentals
    oct  <- parseOctaviation <* spaces
    dur  <- parseDuration
    let dia  = fromJust (name `elemIndex` notes)
    let note = ((toMod dia, acc), fromJust oct)
    return (Value . Music . Unit $ (Just note, fromJust dur))
    where notes = "1234567"

parseOctaviation :: Suihei (Maybe Octave)
parseOctaviation = do
    oct <- getOctaviation
    option oct parseLimiters
    where parseLimiters :: Suihei (Maybe Octave)
          parseLimiters = do
              _   <- symbol "|"
              oct <- nat
              sym <- optionMaybe (symbol "|")
              case sym of
                  -- Updates the active propagated octaviation.
                  Nothing -> modifyOctaviation (Just oct) >> returnM oct
                  -- Otherwise, returns the sealed octaviation with no state alteration.
                  Just _  -> returnM oct

parseRest :: Suihei Expression
parseRest = do
    _ <- symbol "*"
    d <- parseDuration
    (return . Value . Music . Unit) (Nothing, fromJust d)

parseKey :: Suihei Key
parseKey = do
    a <- parseAccidentals
    m  <- choice (map string' analyses)
    let dia = fromJust (map toLower m `elemIndex` reverse (take 7 analyses))
    let key = ((toMod dia, a), if map toLower m /= m then Maj else Min)
    return key
    where analyses = [ "vii", "vi", "v", "iv", "iii", "ii", "i"
                     , "VII", "VI", "V", "IV", "III", "II", "I" ]

parseAnalysis :: Suihei Expression
parseAnalysis = try parseShift <|> parseSymbol

parseShift :: Suihei Expression
parseShift = do
    key <- parseKey <* spaces
    reservedOp ":"  <* spaces
    e   <- parseAnalysis
    (return . Value . Progression . Unit) (extract e key)
    where extract :: Expression -> Key -> TAnalysis
          extract (Value (Progression (Unit (a, d)))) key = (Shift key a, d)
          extract _ _ = undefined

parseSymbol :: Suihei Expression
parseSymbol = do
    key <- parseKey
    ss  <- parseVariationSugar       <* spaces
    vs  <- option [] parseVariations <* spaces
    d   <- parseDuration
    let analysis = Analysis key (ss ++ vs)
    (return . Value . Progression . Unit) (analysis, fromJust d)

parseVariationSugar :: Suihei [Variation]
parseVariationSugar = do
    dim <- option [] (char 'o' >> return [Dim])
    aug <- option [] (char '+' >> return [Aug])
    sv  <- option [] (char '7' >> return [Add 7])
    sn  <- option [] (char '/' >> parseKey >>= \key -> return [Snd key])
    return (dim ++ aug ++ sv ++ sn)

parseVariations :: Suihei [Variation]
parseVariations = do
    vars'  <- getVariations
    vars'' <- option Nothing (symbol "|" >> sepBy parseVariation (symbol ",") >>= \vars -> returnM vars)
    case vars'' of
        Nothing   -> return (fromJust vars')
        Just vars -> modifyVariations vars'' >> return vars
    where parseVariation :: Suihei Variation
          parseVariation = choice [ char 'o'     >> return Dim
                                  , char '+'     >> return Aug
                                  , char '/'     >> parseKey >>= \key -> return (Snd key)
                                  , char 'i'     >> int      >>= \n   -> return (Inv n)
                                  , char 'a'     >> int      >>= \n   -> return (Add n)
                                  , string' "no" >> int      >>= \n   -> return (Sub n) ]

parseDuration :: Suihei (Maybe Duration)
parseDuration = do
    dur' <- getDuration
    res <- option Nothing (parseDurationLiteral
                      <|> (parseDurationVariable >>= \dur1  ->
                           parseDots dur1        >>= \dur2 ->
                           returnM (fromJust dur1 + fromJust dur2))) <* spaces
    case res of
        -- Returns the sealed duration with no state alteration.
        Nothing -> return dur'
        -- Otherwise, updates the active propagated duration.
        Just _  -> modifyDuration res >> return res 
    where parseDurationVariable :: Suihei (Maybe Duration)
          parseDurationVariable = choice [ symbol "w" >> returnM (1 % 1)
                                         , symbol "h" >> returnM (1 % 2)
                                         , symbol "q" >> returnM (1 % 4)
                                         , symbol "e" >> returnM (1 % 8)
                                         , symbol "s" >> returnM (1 % 16)
                                         , symbol "t" >> returnM (1 % 32)]
          parseDots :: Maybe Duration -> Suihei (Maybe Duration)
          parseDots d' = do
              ds <- many (char '.')
              case d' of
                  Nothing -> parserZero
                  Just d  -> if null ds
                             then returnM 0
                             else returnM (d * sum [1 % (2 * i) | i <- [1..length ds]])
          parseDurationLiteral :: Suihei (Maybe Duration)
          parseDurationLiteral = int        >>= \num ->
                                 symbol "/" >>
                                 int        >>= \den ->
                                 returnM (num % den)

parseVariable :: Suihei Expression
parseVariable = Variable <$> ident

parseOut :: Suihei Statement
parseOut = do
    reserved "out"
    e <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    return (Eff (Out e))

parseOutf :: Suihei Statement
parseOutf = do
    reserved "out"
    e   <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    reserved "in"
    key <- choice [parens parseKey, parseKey] <* spaces
    return (Eff (Outf key e))

parseMidi :: Suihei Statement
parseMidi = do
    reserved "midi"
    e <- choice [parens parseExpression, parens parseLiteral, parseVariable]
    return (Eff (Midi e))

parseStatement :: Suihei Statement
parseStatement = (try parseEffect <|> parseDefinition) <* eof
    where parseDefinition = ident           >>= \x ->
                            reservedOp "="  >>
                            parseExpression >>= \e ->
                            return (Def x e)
          parseEffect = choice [try parseOutf, parseOut, parseMidi]

parseLn :: String -> Either String Statement
parseLn ln = case runParserT parseStatement empty "" ln of
    Identity (Left err) -> Left (show err)
    Identity (Right st) -> Right st