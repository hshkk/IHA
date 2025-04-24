module Calla.Data.Chromatic (
    Diatone   (..),
    Diatonic  (..),
    Semitone  (..),
    Chromatic (..)
) where

-- | Represents the pitches of a diatonic scale.
data Diatone = D1 | D2 | D3 | D4 | D5 | D6 | D7 deriving (Eq, Enum, Ord)
-- | Represents musical objects that are diatonic in nature.
data Diatonic a = D Diatone a deriving (Eq, Ord)

-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
data Semitone = Flat | Nat | Sharp deriving (Eq, Ord)
-- | Represents musical objects that are chromatic in nature.
data Chromatic a = C Semitone (Diatonic a) deriving (Eq, Ord)