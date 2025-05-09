{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Calla.Data.Chromatic (
    Diatone,
    i, ii, iii, iv, v, vi, vii,
    diatones,
    Accidental,
    dflat, flat, nat, sharp, dsharp,
    Chromatic,
    Semitone
) where

import Calla.Data.Modular (Mod)

-- | Represents the pitches of a diatonic scale.
type Diatone = Int `Mod` 7

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

-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
type Accidental = Int

dflat, flat, nat, sharp, dsharp :: Accidental
dflat  = -2
flat   = -1
nat    = 0
sharp  = 1
dsharp = 2

-- | Represents musical objects that are chromatic in nature.
type Chromatic a = (Accidental, Diatone, a)

type Semitone = Int `Mod` 12