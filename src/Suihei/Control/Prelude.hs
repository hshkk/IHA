{-# LANGUAGE FlexibleInstances #-}

module Suihei.Control.Prelude (
    inMode,
    Semitonal (..),
    occurrences,
    Trans (..),
    scale, finite, relative,
    interval, secundal, tertian, quartal,
    triad,
    tetrad,
    octaviate,
    rotatedBy, everyNth
) where

import Suihei.Data.Chromatic (
    Diatone, diatones,
    Accidental,
    Enharmonic,
    Semitone,
    accFrom, diaFrom)
import Suihei.Data.Modular (toMod, fromMod)
import Suihei.Data.Chordal (
    Key, Mode (..))
import Suihei.Data.Notal (Note, Octave)
import Suihei.Data.Composition (
    Composition,
    Music, foldM)

import Data.Bifunctor (second, first)
import Suihei.Data.Temporal (Temporal)

inMode :: Diatone -> Mode -> Semitone
inMode dia Maj = [0, 2, 4, 5, 7, 9, 11] !! fromMod dia
inMode dia Min = [0, 2, 3, 5, 7, 8, 10] !! fromMod dia

class Semitonal a where
    pitchClassOf :: a -> Semitone

instance Semitonal Enharmonic where
    pitchClassOf (dia, acc) = dia `inMode` Maj + toMod acc

instance Semitonal Note where
    pitchClassOf (enh, _) = pitchClassOf enh

occurrences :: Music -> [(Enharmonic, Int)]
occurrences = merge . foldM (ins . fst) (++) (++)
    where ins :: Maybe Note -> [(Enharmonic, Int)]
          ins Nothing  = []
          ins (Just n) = [(fst n, 1)]
          merge :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
          merge = foldr (\(a, b) xs -> case lookup a xs of
                                           Just o  -> (a, o + b) : filter ((/= a) . fst) xs
                                           Nothing -> (a, b) : xs) []

class Trans a where
    trans :: a -> Key -> a

instance Trans Key where
    trans key key' = let acc = accFrom key'
                         enh = second (+ acc) (finite key !! fromMod (diaFrom key'))
                         mode = snd key
                     in (enh, mode)

instance Trans Note where
    trans note key = let acc = accFrom key
                         enh = second (+ acc) (finite (fst note, Maj) !! fromMod (diaFrom key))
                     in -- If there was a wraparound:
                        if fromMod (diaFrom note) + fromMod (diaFrom key) > 6
                        then (enh, snd note + 1)
                        else (enh, snd note)

instance Trans a => Trans (Maybe a) where
    trans m key = flip trans key <$> m

instance Trans a => Trans (Temporal a) where
    trans t key = first (`trans` key) t

instance Trans a => Trans (Composition a) where
    trans c key = flip trans key <$> c

-- | Computes an infinite list representing an infinite relative scale.
-- 
-- Relative to C major: @[C, ..., B, ...] :: [Enharmonic]@
-- >>> scale ((0, 0), Maj)
-- [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), ...]
-- 
-- Relative to C major: @[Eb, F, G, Ab, Bb, C, D, ...] :: [Enharmonic]@
-- >>> scale ((2, -1), Maj)
-- [(2, -1), (3, 0), (4, 0), (5, -1), (6, -1), (0, 0), (1, 0), ...]
scale :: Key -> [Enharmonic]
scale (enh, mode)
    = let root        = pitchClassOf enh
          intervals   = diatones `rotatedBy` fromMod (fst enh)
          actuals     = actual root mode <$> diatones
          accidentals = zipWith deviation intervals actuals
          in cycle (zip intervals accidentals)
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Semitone -> Mode -> Diatone -> Semitone
            actual root mode dia = root + (dia `inMode` mode)
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

-- | Computes a finite list representing a finite relative scale of seven notes.
finite :: Key -> [Enharmonic]
finite = take 7 . scale

relative :: Key -> Key
relative key@(_, Maj) = (scale key !! 5, Min)
relative key@(_, Min) = (scale key !! 2, Maj)

-- | Computes an infinite list representing continuously stacked intervals.
-- For instance, @interval 3@ returns an infinite list of continuously stacked thirds.
interval :: Int -> Key -> [Enharmonic]
interval i = everyNth (i - 1) . scale

-- | Computes an infinite list representing continuously stacked seconds.
-- @secundal@ is equivalent to @scale@.
secundal :: Key -> [Enharmonic]
secundal = interval 2

-- | Computes an infinite list representing continuously stacked thirds.
tertian :: Key -> [Enharmonic]
tertian = interval 3

-- | Computes an infinite list representing continuously stacked fourths.
quartal :: Key -> [Enharmonic]
quartal = interval 4

triad :: Key -> [Enharmonic]
triad = take 3 . tertian

tetrad :: Key -> [Enharmonic]
tetrad = take 4 . tertian

octaviate :: Octave -> [Enharmonic] -> [Note]
octaviate oct enhs =
    zip enhs (scanl (\c (enh, enh') -> if enh' < enh
                                       then c + 1
                                       else c) oct (zip enhs (tail enhs)))

rotatedBy :: [a] -> Int -> [a]
rotatedBy [] _ = []
rotatedBy xs n = zipWith const xs' xs
    where xs' = (drop n . cycle) xs

everyNth :: Int -> [a] -> [a]
everyNth n = map head . takeWhile (not . null) . iterate (drop n)