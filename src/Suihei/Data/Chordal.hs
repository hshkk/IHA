{-# LANGUAGE PatternSynonyms #-}

module Suihei.Data.Chordal (
    Mode (..), Key,
    Variation (..),
    pattern Dim, pattern Aug,
    Analysis, pattern Analysis, pattern Shift,
    TAnalysis
) where

import Suihei.Data.Chromatic (Accidental, Chromatic)
import Suihei.Data.Temporal (Temporal)

import Control.Exception (assert)
import Data.Function (on, (&))
import Data.List (nub, sort)

data Mode = Maj | Min deriving (Eq, Ord)
type Key  = Chromatic Mode

-- | Represents the set of possible chord suffixes, or *variations*.
--
-- [Application] Refers to secondary chords "borrowed" from other keys.
-- [Inversion] Rearranges the order of notes in the chord.
-- [Adjustment] Adjusts a tone by an accidental.
-- [Addition] Adds a tone to the chord.
-- [Omission] Omits a tone from the chord.
data Variation = Snd Key
               | Inv Int
               | Adj Int Accidental
               | Add Int | Sub Int
               deriving (Eq, Ord)

pattern Dim :: Variation
pattern Dim = Adj 5 (-1)

pattern Aug :: Variation
pattern Aug = Adj 5 1

-- | Transforms a list of variations into an ordered set.
-- 
-- Then, aseertions are made such that: 
--  - Diminishment and augmentation may not coexist.
--  - Multiple inversions may not coexist.
--  - Multiple secondaries may not coexist.
--
-- >>> V $ Set.fromList [Dim]
-- fromList [Dim]
--
-- >>> V $ Set.fromList [Dim, Aug]
-- Assertion failed
verify :: [Variation] -> [Variation]
verify vars = let vars' = sort (nub vars) in
    vars' & assert (on (||) (`notElem` vars') Dim Aug)
          & assert (length [i | i@(Inv _) <- vars'] <= 1)
          & assert (length [s | s@(Snd _) <- vars'] <= 1)

-- | Represents the symbols of Roman numeric analysis as a 3-tuple of:
--
-- [Key] The denotation of scale degree and mode.
-- [Modifications] Diminutions, augmentations, added/omitted tones, inversions, and secondary chords.
--
-- For example, @viio7@ would be encoded as @(7, L, {Dim, Add 7})@.
data Analysis = A_ Key [Variation] | Shift Key Analysis

{-# COMPLETE Analysis, Shift #-}

-- | Constructs an analysis with an ordered set of variations.
pattern Analysis :: Key -> [Variation] -> Analysis
pattern Analysis key vars  <- A_ key vars
    where Analysis key vars = A_ key (verify vars)

type TAnalysis = Temporal Analysis