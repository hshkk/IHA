{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Chordal (
    Mode (..), Key,
    Variation (..),
    Analysis, pattern Analysis
) where

import Control.Exception (assert)
import Data.Function (on, (&))
import Data.List (nub, sort)

import Calla.Data.Chromatic

data Mode = Maj | Min deriving (Eq, Ord)
type Key  = Chromatic Mode

-- | Represents the set of possible chord suffixes, or *variations*.
--
-- [Diminution] Lowers the fifth by a semitone.
-- [Augmentation] Raises the fifth by a semitone.
-- [Addition] Adds a tone to the chord.
-- [Omission] Omits a tone from the chord.
-- [Inversion] Rearranges the order of notes in the chord.
-- [Application] Refers to secondary chords "borrowed" from other keys.
data Variation = Dim | Aug
               | Snd Key
               | Add Int | Sub Int
               | Inv Int
               deriving (Eq, Ord)

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
verify vs = let vs' = sort (nub vs) in
    vs' & assert (on (||) (`notElem` vs') Dim Aug)
        & assert (length [i | i@(Inv _) <- vs'] <= 1)
        & assert (length [s | s@(Snd _) <- vs'] <= 1)

-- | Represents the symbols of Roman numeric analysis as a 3-tuple of:
--
-- [Key] The denotation of scale degree and mode.
-- [Modifications] Diminutions, augmentations, added/omitted tones, inversions, and secondary chords.
--
-- For example, @viio7@ would be encoded as @(7, L, {Dim, Add 7})@.
data Analysis = A_ Key [Variation]

{-# COMPLETE Analysis #-}

-- | Constructs an analysis with an ordered set of variations.
pattern Analysis :: Key -> [Variation] -> Analysis
pattern Analysis k vs  <- A_ k vs
    where Analysis k vs = A_ k (verify vs)