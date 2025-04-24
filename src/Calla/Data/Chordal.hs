{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Chordal (
    Key, Mode (..),
    Analysis, pattern Analysis, analysis,
    Variation (..),
    Progression
) where

import Control.Exception (assert)
import Data.Function (on)
import Data.Set (Set)

import qualified Data.Set as Set

import Calla.Data.Chromatic (Chromatic)
import Calla.Data.Temporal (Duration, Copuntal)

type Key  = Chromatic Mode
data Mode = Maj | Min 
    deriving (Ord, Eq)

-- | Represents the symbols of Roman numeric analysis as a 3-tuple of:
--
-- [Key] The denotation of scale degree and mode.
-- [Modifications] Diminutions, augmentations, added/omitted tones, inversions, and secondary chords.
--
-- For example, @viio7@ would be encoded as @(7, L, {Dim, Add 7})@.
data Analysis = Analysis Key (Set Variation) deriving Eq

-- | Represents the set of possible chord suffixes, or *variations*.
--
-- [Diminution] Lowers the fifth by a semitone.
-- [Augmentation] Raises the fifth by a semitone.
-- [Addition] Adds a tone to the chord.
-- [Omission] Omits a tone from the chord.
-- [Inversion] Rearranges the order of notes in the chord.
-- [Application] Refers to secondary chords "borrowed" from other keys.
data Variation = Dim | Aug
               | Add Int | Sub Int
               | Inv Int
               | Snd Key
               deriving (Ord, Eq)

-- | Constructs a Roman numeric analysis with notational restrictions.
analysis :: Key -> Set Variation -> Analysis
analysis x vs = Analysis x (V vs)

-- | Restricts a set of variations such that diminishment and augmentation may not coexist.
--
-- >>> V $ Set.fromList [Dim]
-- fromList [Dim]
--
-- >>> V $ Set.fromList [Dim, Aug]
-- Assertion failed
pattern V :: Set Variation -> Set Variation
pattern V vs <- vs where
    V vs = assert (on (||) (`Set.notMember` vs) Dim Aug) vs

type Progression = Copuntal Analysis