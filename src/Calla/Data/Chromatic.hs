{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Calla.Data.Chromatic (
    Diatone     (..),
    diatones,
    Diatonic    (..), 
    Accidental,
    dflat, flat, nat, sharp, dsharp,
    Chromatic   (..),
    Semitone,
    (%+), (%-), (%%+), (%%-)
) where

import Calla.Data.Modular (type (%%), fromMod, toMod)

-- | Represents the pitches of a diatonic scale.
data Diatone = D1 | D2 | D3 | D4 | D5 | D6 | D7
    deriving (Eq, Bounded, Enum, Ord, Show)

diatones :: [Diatone]
diatones = enumFromTo D1 D7

-- | Represents musical objects that are diatonic in nature.
data Diatonic a = D Diatone a deriving (Eq, Ord)

-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
type Accidental = Int

dflat, flat, nat, sharp, dsharp :: Accidental
dflat  = -2
flat   = -1
nat    = 0
sharp  = 1
dsharp = 2

-- | Represents musical objects that are chromatic in nature.
data Chromatic a = C Accidental (Diatonic a) deriving (Eq, Ord)

-- 
type Semitone = (Diatone, Accidental)

type ModularDiatone_ = Int %% 7
toModularDiatone_ :: Diatone -> ModularDiatone_
toModularDiatone_ = succ . toMod . fromEnum

(%+) :: Diatone -> Diatone -> Diatone
(%+) d1 d2 = (toEnum . fromMod . pred) (toModularDiatone_ d1 + toModularDiatone_ d2)

(%-) :: Diatone -> Diatone -> Diatone
(%-) d1 d2 = (toEnum . fromMod . pred) (toModularDiatone_ d1 - toModularDiatone_ d2)

type ModularSemitone_ = Int %% 12
toModularSemitone_ :: Int -> ModularSemitone_
toModularSemitone_ = toMod

(%%+) :: Int -> Int -> Int
(%%+) s1 s2 = fromMod (toModularSemitone_ s1 + toModularSemitone_ s2)

(%%-) :: Int -> Int -> Int
(%%-) s1 s2 = fromMod (toModularSemitone_ s1 - toModularSemitone_ s2)