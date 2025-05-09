module Calla.Control.Prelude (
    inMode, scale, triad
) where

import Calla.Data.Chromatic (
    Diatone, diatones,
    Accidental, Chromatic,
    Semitone)
import Calla.Data.Chordal (
    Key, Mode (..))
import Calla.Data.Modular (toMod, fromMod)

inMode :: Diatone -> Mode -> Semitone
inMode d Maj = [0, 2, 4, 5, 7, 9, 11] !! fromMod d
inMode d Min = [0, 2, 3, 5, 7, 8, 10] !! fromMod d

-- | Computes an infinite list of functions [a -> Chromatic a] representing an infinite relative scale.
-- 
-- Relative to C major: [C4, ..., B4, ...] :: [Note]
-- >>> zipWith ($) (scale (0, 0, Maj)) (repeat 4 :: [Octave])
-- [(0,0,4), (0,1,4), (0,2,4), (0,3,4), (0,4,4), (0,5,4), (0,6,4), ...]
-- 
-- Relative to C major: [Eb, Fm, G, Abm, Bb, Cm, D, ...] :: [Key]
-- >>> zipWith ($) (scale (-1, 2, Maj)) (cycle [Maj, Min] :: [Mode])
-- [(-1,2,Maj), (0,3,Min), (0,4,Maj), (-1,5,Min), (-1,6,Maj), (0,0,Min), (0,1,Maj), ...]
scale :: Key -> [a -> Chromatic a]
scale (a, d, m)
    = let root        = d `inMode` m + toMod a
          intervals   = diatones `rotatedBy` fromMod d
          actuals     = actual root m <$> diatones
          accidentals = zipWith deviation intervals actuals
          in cycle (zipWith (,,) accidentals intervals)
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Semitone -> Mode -> Diatone -> Semitone
            actual r m d = r + d `inMode` m
            -- Computes the pitch class deviation of s with respect to the "reference" pitch class d.
            deviation :: Diatone -> Semitone -> Accidental
            deviation d s =
                let raw = fromMod s - fromMod (d `inMode` Maj)
                    clamp | raw > 6   = raw - 12
                          | raw < -6  = raw + 12
                          | otherwise = raw
                in clamp

triad :: Key -> [a -> Chromatic a]
triad = take 3 . everyNth 2 . scale

rotatedBy :: [a] -> Int -> [a]
rotatedBy [] _ = []
rotatedBy xs n = zipWith const xs' xs
    where xs' = (drop n . cycle) xs

everyNth :: Int -> [a] -> [a]
everyNth n = map head . takeWhile (not . null) . iterate (drop n)