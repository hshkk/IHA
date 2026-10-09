module IHA.Text.Ancillary.Combinator (
    (?<|>), returnM,
    fromJustVariations,
    fromJustOctaviation,
    fromJustDuration
) where

import IHA.Data.Chordal (Variation)
import IHA.Data.Notal (Octave)
import IHA.Data.Temporal (Duration)

import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec (
    GenParser, 
    try, (<|>))

import Data.Functor.Identity (Identity)

(?<|>) :: GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
p1 ?<|> p2 = try p1 <|> p2
infixr 1 ?<|>

returnM :: Monad m => a -> m (Maybe a)
returnM a = return (Just a)

fromJustVariations :: Maybe [Variation] -> [Variation]
fromJustVariations (Just vars) = vars
fromJustVariations Nothing     = error $ "Text.Ancillary.Combinator: "
                                      ++ "Undefined variation set"

fromJustOctaviation :: Maybe Octave -> Octave
fromJustOctaviation (Just oct) = oct
fromJustOctaviation Nothing    = error $ "Text.Ancillary.Combinator: "
                                      ++ "Undefined octaviation"

fromJustDuration :: Maybe Duration -> Duration
fromJustDuration (Just dur) = dur
fromJustDuration Nothing    = error $ "Text.Ancillary.Combinator: "
                                   ++ "Undefined duration"