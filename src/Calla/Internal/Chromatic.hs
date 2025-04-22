{-# LANGUAGE PatternSynonyms #-}

module Calla.Internal.Chromatic (
    Diatonic  (..),
    Diatone   (..),
    Chromatic (..),
    Semitone  (..),
    Key, Mode (..),
    Note, 
    Roma, pattern Roma, roma,
    Color (..),
    Progression
) where

import Control.Exception (assert)
import Data.Function (on)
import Data.Set (Set)

import qualified Data.Set as Set

data Diatonic x = D Diatone x 
    deriving (Ord, Eq)

-- | Represents the pitches of a diatonic scale.
data Diatone = D1 | D2 | D3 | D4 | D5 | D6 | D7 
    deriving (Ord, Eq)

data Chromatic x = X Semitone (Diatonic x) 
    deriving (Ord, Eq)

data Semitone = Flat | Nat | Sharp 
    deriving (Ord, Eq)

type Key  = Chromatic Mode
data Mode = Maj | Min 
    deriving (Ord, Eq)

type Note   = Chromatic Octave
type Octave = Int

-- | Represents the symbols of Roman numeric analysis as a 3-tuple of:
--
-- [Degree] The distance from the tonic.
-- [Mode] The denotation of a major or minor triad.
-- [Modifications] Diminutions, augmentations, added/omitted tones, inversions, and secondary chords.
--
-- For example, @viio7@ would be encoded as @(7, L, {Dim, Add 7})@.
data Roma  = Roma Key (Set Color) deriving Eq

data Color = Dim | Aug
           | Add Int | Sub Int
           | Inv Int
           | Snd Key
           deriving (Ord, Eq)

-- | Constructs a Roman numeric analysis with notational restrictions.
roma :: Key -> Set Color -> Roma
roma x cs = Roma x (C cs)

-- | Restricts a set of modifications such that diminishment and augmentation may not coexist.
--
-- >>> M $ Set.fromList [Dim]
-- fromList [Dim]
--
-- >>> M $ Set.fromList [Dim, Aug]
-- Assertion failed
pattern C :: Set Color -> Set Color
pattern C cs <- cs where
    C cs = assert (on (||) (`Set.notMember` cs) Dim Aug) cs

type Progression = [Roma]