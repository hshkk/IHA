{-# LANGUAGE LambdaCase #-}

module Suihei.Control.Prelude (
    pitchClass, pitchClasses,
    inMode, 
    scale,
    interval,
    secundal, tertian, quartal,
    triad, tetrad
) where

import Suihei.Data.Chromatic (
    Diatone, diatones,
    Accidental, Chromatic,
    Semitone)
import Suihei.Data.Modular (toMod, fromMod)
import Suihei.Data.Chordal (
    Key, Mode (..))
import Suihei.Data.Notal (Note, Octave)
import Suihei.Data.Composition (Music, foldM)

pitchClass :: Note -> Semitone
pitchClass ((d, _), a) = d `inMode` Maj + toMod a

-- | Retrieves the pitch classes of the constituent notes of a single parallel composition while preserving octaviation.
-- @pitchClasses@ is undefined for serial compositions, including those nested within parallel compositions.
pitchClasses :: Music -> [(Semitone, Octave)]
pitchClasses = foldM (\case { (Nothing, _) -> []; 
                              (Just n,  _) -> [(pitchClass n, (snd . fst) n)] }) 
                     undefined 
                     (++)

-- (((d, o), a), d')

inMode :: Diatone -> Mode -> Semitone
inMode d Maj = [0, 2, 4, 5, 7, 9, 11] !! fromMod d
inMode d Min = [0, 2, 3, 5, 7, 8, 10] !! fromMod d

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing an infinite relative scale.
-- 
-- Relative to C major: @[C4, ..., B4, ...] :: [Note]@
-- >>> zipWith ($) (scale ((0, Maj), 0)) (repeat 4 :: [Octave])
-- [((0, 4), 0), ((1, 4), 0), ((2, 4), 0), ((3, 4), 0), ((4, 4), 0), ((5, 4), 0), ((6, 4), 0), ...]
-- 
-- Relative to C major: @[Eb, Fm, G, Abm, Bb, Cm, D, ...] :: [Key]@
-- >>> zipWith ($) (scale (-1, 2, Maj)) (cycle [Maj, Min] :: [Mode])
-- [((2, Maj), -1), ((3, Min), 0), ((4, Maj), 0), ((5, Min), -1), ((6, Maj), -1), ((0, Min), 0), ((1, Maj), 0), ...]
scale :: Key -> [a -> Chromatic a]
scale ((d, m), a)
    = let root        = d `inMode` m + toMod a
          intervals   = diatones `rotatedBy` fromMod d
          actuals     = actual root m <$> diatones
          accidentals = zipWith deviation intervals actuals
          in cycle (zipWith (\i a x -> ((i, x), a)) intervals accidentals)
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Semitone -> Mode -> Diatone -> Semitone
            actual r m d = r + d `inMode` m
            -- Computes the pitch class deviation of s with respect to the "reference" pitch class d.
            deviation :: Diatone -> Semitone -> Accidental
            deviation d s =
                let raw = fromMod s - fromMod (d `inMode` Maj)
                    -- Clamping the raw deviation prevents long accidental chains with simpler equivalents.
                    -- e.g., with respect to pitch class, 11 sharps is equivalent to 1 flat.
                    clamp | raw > 6   = raw - 12
                          | raw < -6  = raw + 12
                          | otherwise = raw
                in clamp

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked intervals.
-- For instance, @interval 3@ returns an infinite list of continuously stacked thirds.
interval :: Int -> Key -> [a -> Chromatic a]
interval i = everyNth (i - 1) . scale

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked seconds.
-- @secundal@ is equivalent to @scale@.
secundal :: Key -> [a -> Chromatic a]
secundal = interval 2

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked thirds.
tertian :: Key -> [a -> Chromatic a]
tertian = interval 3

-- | Computes an infinite list of functions @[a -> Chromatic a]@ representing continuously stacked fourths.
quartal :: Key -> [a -> Chromatic a]
quartal = interval 4

triad :: Key -> [a -> Chromatic a]
triad = take 3 . tertian

tetrad :: Key -> [a -> Chromatic a]
tetrad = take 4 . tertian

rotatedBy :: [a] -> Int -> [a]
rotatedBy [] _ = []
rotatedBy xs n = zipWith const xs' xs
    where xs' = (drop n . cycle) xs

everyNth :: Int -> [a] -> [a]
everyNth n = map head . takeWhile (not . null) . iterate (drop n)