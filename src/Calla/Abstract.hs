{-# LANGUAGE PatternSynonyms #-}

module Calla.Abstract where

import Control.Exception (assert)
import Data.Function (on)
import Data.Ratio (Ratio)
import Data.Set (Set)

import qualified Data.Set as Set

data Munit a
    = Unit a Duration
    -- ...
    | Munit a :|: Munit a
    deriving (Show, Eq)

instance Functor Munit where
    fmap f (Unit u n)  = Unit (f u) n
    -- ...
    fmap f (m1 :|: m2) = fmap f m1 :|: fmap f m2

type Duration = Ratio Int

-- | Represents the symbols of Roman numeric analysis as a 3-tuple of:
--
-- [Degree] The distance from the tonic.
-- [Case] The denotation of a major or minor triad.
-- [Modifications] Diminishments, augmentations, added tones, and inversions.
--
-- For example, @viio7@ would be encoded as @(7, L, {Dim, Add 7})@.
type Roma = (Int, Case, Set Mod)

-- | Constructs a Roman numeric analysis with notational restrictions.
roma :: Int -> Case -> [Mod] -> Roma
roma x c mds = (D x, c, M $ Set.fromList mds)

-- | Represents degrees @I, ..., VII@ as an integer restricted to [1, 7].
--
-- >>> D 4
-- 4
--
-- >>> D 8
-- Assertion failed
pattern D :: Int -> Int
pattern D x <- x where
    D x = assert (x >= 1 && x <= 7) x

-- | Represents the denotation of major @I, ..., VII@, and minor @i, ..., vii@.
data Case = U | L
    deriving (Show, Eq)

-- | Represents various modifications on chords, such as:
--
-- [Diminishments] e.g. @viio@.
-- [Augmentations] e.g. @IV+@.
-- [Added tones] e.g. @V7@.
-- [Inversions] e.g. @I^6, I^6_4, ...@.
data Mod = Dim | Aug | Add Int | Inv Int
    deriving (Show, Eq, Ord)

-- | Restricts a set of modifications such that diminishment and augmentation may not coexist.
pattern M :: Set Mod -> Set Mod
pattern M mds <- mds where
    M mds = assert (on (||) (`Set.notMember` mds) Dim Aug) mds

-- ...