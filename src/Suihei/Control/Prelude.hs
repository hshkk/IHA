{-# LANGUAGE FlexibleInstances #-}

module Suihei.Control.Prelude (
    inMode, 
    Semitonal (..),
    occurrences,
    scale,
    interval, secundal, tertian, quartal,
    triad, tetrad,
    applyEachTo
) where

import Suihei.Data.Chromatic (
    Diatone, diatones,
    Accidental, 
    Enharmonic,
    Semitone)
import Suihei.Data.Modular (toMod, fromMod)
import Suihei.Data.Chordal (
    Key, Mode (..))
import Suihei.Data.Notal (Note)
import Suihei.Data.Composition (Music, foldM)

inMode :: Diatone -> Mode -> Semitone
inMode dia Maj = [0, 2, 4, 5, 7, 9, 11] !! fromMod dia
inMode dia Min = [0, 2, 3, 5, 7, 8, 10] !! fromMod dia

class Semitonal a where
    pitchClassOf :: a -> Semitone

instance Semitonal Key where
    pitchClassOf (enh, mode) = 
        let dia = fst enh
            acc = snd enh in toMod (fromMod (dia `inMode` mode) + acc)

instance Semitonal Note where
    pitchClassOf (enh, _) = pitchClassOf (enh, Maj)

occurrences :: Music -> [(Enharmonic, Int)]
occurrences = merge . foldM (ins . fst) (++) (++)
    where ins :: Maybe Note -> [(Enharmonic, Int)]
          ins Nothing  = []
          ins (Just n) = [(fst n, 1)]
          merge :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
          merge = foldr (\(a, b) xs -> case lookup a xs of
                                           Just o  -> (a, o + b) : filter ((/= a) . fst) xs
                                           Nothing -> (a, b) : xs) []

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing an infinite relative scale.
-- 
-- Relative to C major: @[C4, ..., B4, ...] :: [Note]@
-- >>> scale ((0, Maj), 0) `applyEachTo` (repeat 4 :: [Octave])
-- [((0, 4), 0), ((1, 4), 0), ((2, 4), 0), ((3, 4), 0), ((4, 4), 0), ((5, 4), 0), ((6, 4), 0), ...]
-- 
-- Relative to C major: @[Eb, Fm, G, Abm, Bb, Cm, D, ...] :: [Key]@
-- >>> scale (-1, 2, Maj) `applyEachTo` (cycle [Maj, Min] :: [Mode])
-- [((2, Maj), -1), ((3, Min), 0), ((4, Maj), 0), ((5, Min), -1), ((6, Maj), -1), ((0, Min), 0), ((1, Maj), 0), ...]
--
-- @applyEachTo@ is equivalent to @zipWith ($)@.
scale :: Key -> [Enharmonic]
scale key@((dia, _), mode)
    = let root        = pitchClassOf key
          intervals   = diatones `rotatedBy` fromMod dia
          actuals     = actual root mode <$> diatones
          accidentals = zipWith deviation intervals actuals
          in cycle (zip intervals accidentals)
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Semitone -> Mode -> Diatone -> Semitone
            actual root mode dia = root + dia `inMode` mode
            -- Computes the pitch class deviation of s with respect to the "reference" pitch class d.
            deviation :: Diatone -> Semitone -> Accidental
            deviation dia semi =
                let raw = fromMod semi - fromMod (dia `inMode` Maj)
                    -- Clamping the raw deviation prevents long accidental chains with simpler equivalents.
                    -- e.g., with respect to pitch class, 11 sharps is equivalent to 1 flat.
                    clamp | raw > 6   = raw - 12
                          | raw < -6  = raw + 12
                          | otherwise = raw
                in clamp

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked intervals.
-- For instance, @interval 3@ returns an infinite list of continuously stacked thirds.
interval :: Int -> Key -> [Enharmonic]
interval i = everyNth (i - 1) . scale

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked seconds.
-- @secundal@ is equivalent to @scale@.
secundal :: Key -> [Enharmonic]
secundal = interval 2

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked thirds.
tertian :: Key -> [Enharmonic]
tertian = interval 3

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked fourths.
quartal :: Key -> [Enharmonic]
quartal = interval 4

triad :: Key -> [Enharmonic]
triad = take 3 . tertian

tetrad :: Key -> [Enharmonic]
tetrad = take 4 . tertian

applyEachTo :: [a -> b] -> [a] -> [b]
applyEachTo = zipWith ($)

rotatedBy :: [a] -> Int -> [a]
rotatedBy [] _ = []
rotatedBy xs n = zipWith const xs' xs
    where xs' = (drop n . cycle) xs

everyNth :: Int -> [a] -> [a]
everyNth n = map head . takeWhile (not . null) . iterate (drop n)