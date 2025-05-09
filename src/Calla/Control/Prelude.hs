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

scale :: Key -> [a -> Chromatic a]
scale (a, d, m)
    = let root        = d `inMode` m + toMod a
          intervals   = diatones `rotatedBy` fromMod d
          actuals     = actual root m <$> diatones
          accidentals = zipWith deviation intervals actuals
          in zipWith (,,) accidentals intervals
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Semitone -> Mode -> Diatone -> Semitone
            actual r m d = r + (d `inMode` m)
            -- Computes the pitch class deviation of s with respect to the pitch class of d.
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
rotatedBy xs n = zipWith const (drop n . cycle $ xs) xs

everyNth :: Int -> [a] -> [a]
everyNth n = map head . takeWhile (not . null) . iterate (drop n)