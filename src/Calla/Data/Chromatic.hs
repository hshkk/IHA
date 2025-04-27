{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Calla.Data.Chromatic (
    Diatone, diatones,
    i, ii, iii, iv, v, vi, vii,
    Diatonic  (..), 
    Accidental,
    dflat, flat, nat, sharp, dsharp,
    Chromatic (..),
    Semitone
) where

import Calla.Data.Modular (type (%%))

-- | Represents the pitches of a diatonic scale.
type Diatone = Int %% 7

i, ii, iii, iv, v, vi, vii :: Diatone
i   = 0
ii  = 1
iii = 2
iv  = 3
v   = 4
vi  = 5
vii = 6

diatones :: [Diatone]
diatones = [i..vii]

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

type Semitone = Int %% 12