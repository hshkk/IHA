{-# LANGUAGE PatternSynonyms #-}

module Suihei.Text.Parser (
    parseLn
) where

import Suihei.Data.Abstract (
    Statement  (..),
    Expression (..),
    Value      (..),
    Effect     (..))
import Suihei.Data.Chromatic (Accidental)
import Suihei.Data.Modular (toMod)
import Suihei.Data.Chordal (
    Key, Mode (..),
    Analysis  (..), pattern Analysis, TAnalysis,
    Variation (..))
import Suihei.Data.Notal (Octave)
import Suihei.Data.Composition (pattern Unit)
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Lexer
import Suihei.Text.Monads

import Text.Parsec (string', parserZero)
import Text.Parsec.Expr (
    Operator (Infix), Assoc (AssocLeft, AssocRight),
    buildExpressionParser)
import Text.Parsec.Prim (ParsecT, runParserT)
import Text.ParserCombinators.Parsec

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Functor.Identity (
    Identity (Identity))
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio ((%))

(?<|>) :: GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
p1 ?<|> p2 = try p1 <|> p2
infixr 1 ?<|>

parseExpression :: SuiheiParser Expression
parseExpression = buildExpressionParser operatorTable parseTerm

operatorTable :: SuiheiOperatorTable Expression
operatorTable = [ [ ifxl (parseOperator "^"  (:^.))]
                , [ ifxl (parseOperator ">|" (:>|))
                  , ifxl (parseOperator "|<" (:|<))
                  , ifxl (parseOperator ">"  (:>.)) ] ]
    where ifxl op = Infix op AssocLeft
          ifxr op = Infix op AssocRight

parseOperator :: String 
              -> (Expression -> Expression -> Expression) 
              -> SuiheiParser (Expression -> Expression -> Expression)
parseOperator str op = op <$ reservedOp str

parseTerm :: SuiheiParser Expression
parseTerm = parseInfer
       ?<|> parseValue
       ?<|> parens parseExpression

parseInfer :: SuiheiParser Expression
parseInfer = do
    reserved "infer"
    exp <- choice [parens parseExpression, parseVariable]
    return (Infer exp)

parseValue :: SuiheiParser Expression
parseValue = parseLiteral
        ?<|> parseVariable

parseLiteral :: SuiheiParser Expression
parseLiteral = parseAbsoluteNote
          ?<|> parseRelativeNote
          ?<|> parseRest
          ?<|> parseAnalysis

parseAbsoluteNote :: SuiheiParser Expression
parseAbsoluteNote = do
    name <- oneOf absolutes
    acc  <- parseAccidentals
    oct  <- parseAbsoluteNoteSugar <* spaces
       ?<|> parseOctaviationPropagation (returnM =<< nat) <* spaces
    dur  <- parseDuration
    let dia  = fromJust (name `elemIndex` absolutes)
    let note = ((toMod dia, acc), fromJust oct)
    return (Value . Music . Unit $ (Just note, fromJust dur))
    where parseAbsoluteNoteSugar :: SuiheiParser (Maybe Octave)
          parseAbsoluteNoteSugar = do
              oct <- nat
              modifyOctaviation (Just oct)
              returnM oct
          absolutes = "CDEFGAB"

parseRelativeNote :: SuiheiParser Expression
parseRelativeNote = do
    name <- oneOf relatives
    acc  <- parseAccidentals
    oct  <- parseOctaviationPropagation (returnM =<< nat) <* spaces
    dur  <- parseDuration
    let dia  = fromJust (name `elemIndex` relatives)
    let note = ((toMod dia, acc), fromJust oct)
    return (Value . Music . Unit $ (Just note, fromJust dur))
    where relatives = "1234567"

parseRest :: SuiheiParser Expression
parseRest = do
    _ <- symbol "*"
    dur <- parseDuration
    return (Value . Music . Unit $ (Nothing, fromJust dur))

parseAnalysis :: SuiheiParser Expression
parseAnalysis = parseShift ?<|> parseSymbol

parseShift :: SuiheiParser Expression
parseShift = do
    key <- parseKey <* spaces
    reservedOp ":"  <* spaces
    ana <- parseAnalysis
    (return . Value . Progression . Unit) (extract ana key)
    where extract :: Expression -> Key -> TAnalysis
          extract (Value (Progression (Unit (ana, dur)))) key = (Shift key ana, dur)
          extract _ _ = undefined

parseSymbol :: SuiheiParser Expression
parseSymbol = do
    st    <- getVariations
    key   <- parseKey
    vars1 <- parseVariationSugar <* spaces
    vars2 <- parseVariations     <* spaces
    -- If no propagation or list of modifications is present, the parser defaults to no modifications.
    vars' <- if isNothing st && isNothing vars2 then returnM [] else return vars2
    dur   <- parseDuration
    let analysis = Analysis key (vars1 ++ fromJust vars')
    (return . Value . Progression . Unit) (analysis, fromJust dur)

parseVariable :: SuiheiParser Expression
parseVariable = Variable <$> ident

parsePropagation :: SuiheiParser (Maybe a)       -- ^ Retreive from state
                 -> (Maybe a -> SuiheiParser ()) -- ^ Transform the state
                 -> SuiheiParser (Maybe a)       -- ^ Parser
                 -> SuiheiParser (Maybe a)
parsePropagation get modify parser = do
    st  <- get
    option st (parseTermination st <|> parseLimits)
    where parseTermination st = do
              _   <- symbol "*"
              modify Nothing
              return st
          parseLimits = do
              _   <- symbol "|"
              res <- parser
              sym <- optionMaybe (symbol "|")
              unless (isJust sym) (modify res)
              return res

parseOctaviationPropagation :: SuiheiParser (Maybe Octave) -> SuiheiParser (Maybe Octave)
parseOctaviationPropagation = parsePropagation getOctaviation modifyOctaviation

parseVariationPropagation :: SuiheiParser (Maybe [Variation]) -> SuiheiParser (Maybe [Variation])
parseVariationPropagation = parsePropagation getVariations modifyVariations

parseAccidentals :: SuiheiParser Accidental
parseAccidentals = choice [ length          <$> many1 (symbol "#")
                          , negate . length <$> many1 (symbol "b")
                          , return 0]

parseKey :: SuiheiParser Key
parseKey = do
    acc  <- parseAccidentals
    name <- choice (map string' analyses)
    let dia = fromJust (map toLower name `elemIndex` ordered)
    let key = ((toMod dia, acc), if map toLower name /= name then Maj else Min)
    return key
    where analyses = [ "vii", "vi", "v", "iv", "iii", "ii", "i"
                     , "VII", "VI", "V", "IV", "III", "II", "I" ]
          ordered  = reverse (take 7 analyses)

parseVariationSugar :: SuiheiParser [Variation]
parseVariationSugar = do
    dim <- option [] (char 'o' >> return [Dim])
    aug <- option [] (char '+' >> return [Aug])
    sv  <- option [] (char '7' >> return [Add 7])
    sn  <- option [] (char '/' >> parseKey >>= \key -> return [Snd key])
    return (dim ++ aug ++ sv ++ sn)

parseVariations :: SuiheiParser (Maybe [Variation])
parseVariations = option (Just []) (parseVariationPropagation (Just <$> sepBy1 parseVariation (symbol ",")))
    where parseVariation :: SuiheiParser Variation
          parseVariation = choice [ char 'o'     >> return Dim
                                  , char '+'     >> return Aug
                                  , char '/'     >> parseKey >>= \key -> return (Snd key)
                                  , char 'i'     >> int      >>= \n   -> return (Inv n)
                                  , char 'a'     >> int      >>= \n   -> return (Add n)
                                  , string' "no" >> int      >>= \n   -> return (Sub n) ]

parseDuration :: SuiheiParser (Maybe Duration)
parseDuration = do
    dur <- getDuration
    res <- option Nothing (parseDurationLiteral
                      <|> (parseDurationVariable >>= \dur1  ->
                           parseDots dur1        >>= \dur2 ->
                           returnM (fromJust dur1 + fromJust dur2))) <* spaces
    case res of
        -- Returns the sealed duration with no state alteration.
        Nothing -> return dur
        -- Otherwise, updates the active propagated duration.
        Just _  -> modifyDuration res >> return res
    where parseDurationVariable :: SuiheiParser (Maybe Duration)
          parseDurationVariable = choice [ symbol "w" >> returnM (1 % 1)
                                         , symbol "h" >> returnM (1 % 2)
                                         , symbol "q" >> returnM (1 % 4)
                                         , symbol "e" >> returnM (1 % 8)
                                         , symbol "s" >> returnM (1 % 16)
                                         , symbol "t" >> returnM (1 % 32)]
          parseDots :: Maybe Duration -> SuiheiParser (Maybe Duration)
          parseDots dur' = do
              dots <- many (char '.')
              case dur' of
                  Nothing  -> parserZero
                  Just dur -> if null dots
                             then returnM 0
                             else returnM (dur * sum [1 % (2 * i) | i <- [1..length dots]])
          parseDurationLiteral :: SuiheiParser (Maybe Duration)
          parseDurationLiteral = int        >>= \num ->
                                 symbol "/" >>
                                 int        >>= \den ->
                                 returnM (num % den)

parseEffectAbs :: String                 -- ^ Function name
               -> (Expression -> Effect) -- ^ Effect constructor
               -> SuiheiParser Statement
parseEffectAbs fun eff = do
    reserved fun
    exp <- choice [parens parseExpression, parseVariable]
    return (Eff . eff $ exp)

parseEffectCon :: String                        -- ^ Function name
              -> (Key -> Expression -> Effect) -- ^ Effect constructor
              -> SuiheiParser Statement
parseEffectCon fun eff = do
    reserved fun
    exp <- choice [parens parseExpression, parseVariable]
    reserved "in"
    key <- choice [parens parseKey, parseKey] <* spaces
    return (Eff . eff key $ exp)

parseOut :: SuiheiParser Statement
parseOut = parseEffectAbs "out" Out

parseOutf :: SuiheiParser Statement
parseOutf = parseEffectCon "out" Outf

parseMidi :: SuiheiParser Statement
parseMidi = parseEffectCon "midi" Midi

parseStatement :: SuiheiParser Statement
parseStatement = (parseEffect ?<|> parseDefinition) <* eof
    where parseDefinition = ident           >>= \var ->
                            reservedOp "="  >>
                            parseExpression >>= \exp ->
                            return (Def var exp)
          parseEffect = choice [try parseOutf, parseOut, parseMidi]

parseLn :: String -> Either String Statement
parseLn ln = case runParserT parseStatement empty "" ln of
    Identity (Left err) -> Left (show err)
    Identity (Right st) -> Right st