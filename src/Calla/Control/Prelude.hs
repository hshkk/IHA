{-# LANGUAGE PatternSynonyms #-}

module Calla.Control.Prelude (
    unchroma, chroma,
    scale, triad, inMode
) where

import Calla.Data.Chromatic (
    Diatone    (..), Diatonic  (..),
    Accidental (..), Chromatic (..),
    Semitone, (%+), (%%+), (%-), diatones, flat, sharp)
import Calla.Data.Chordal (
    Key, Mode (..),
    Analysis, pattern Analysis,
    Variation (..))

unchroma :: Chromatic a -> (Accidental, Diatone, a)
unchroma (C s (D d a)) = (s, d, a)

chroma :: (Accidental, Diatone, a) -> Chromatic a
chroma (s, d, a) = C s (D d a)

type Class_ = Int

inMode :: Diatone -> Mode -> Class_
inMode d Maj = ionian  !! fromEnum d
    where ionian :: [Class_]
          ionian = [0, 2, 4, 5, 7, 9, 11]
inMode d Min = aeolian !! fromEnum d
    where aeolian :: [Class_]
          aeolian = [0, 2, 3, 5, 7, 8, 10]

scale :: Key -> [Semitone]
scale k
    | (a, d, m) <- unchroma k
    = let root       = d `inMode` m %%+ a
          intervals  = diatones `rotatedBy` fromEnum d
          actuals    = map (actual root m) diatones
          offsets    = zipWith deviation intervals actuals
          in zip intervals offsets
      where -- Computes the actual pitch class of a note, given the root.
            actual :: Class_ -> Mode -> Diatone -> Class_
            actual r m d = r %%+ (d `inMode` m)
            -- Computes the pitch class deviation of c with respect to the pitch class_ of d.
            deviation :: Diatone -> Class_ -> Int
            deviation d c =
                let raw = c - (d `inMode` Maj)
                    clamp | raw > 6   = raw - 12
                          | raw < -6  = raw + 12
                          | otherwise = raw
                in clamp

triad :: Key -> [Semitone]
triad = take 3 . everyNth 2 . scale

rotatedBy :: [a] -> Int -> [a]
rotatedBy [] _ = []
rotatedBy xs n = take (length xs) (drop n (cycle xs))

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n xs = map (xs !!) [0, n..(length xs - 1)]