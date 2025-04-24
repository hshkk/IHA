module Calla.Data.Chromatic (
    Diatonic  (..),
    Diatone   (..),
    Chromatic (..),
    Semitone  (..)
) where

data Diatonic a = D Diatone a 
    deriving (Ord, Eq)
-- | Represents the pitches of a diatonic scale.
data Diatone = D1 | D2 | D3 | D4 | D5 | D6 | D7 
    deriving (Ord, Eq)

data Chromatic a = C Semitone (Diatonic a) 
    deriving (Ord, Eq)
-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
data Semitone = Flat | Nat | Sharp 
    deriving (Ord, Eq)