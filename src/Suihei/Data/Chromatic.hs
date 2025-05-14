{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Suihei.Data.Chromatic (
    Diatone, Semitone,
    Diatonic,
    Accidental,
    Enharmonic,
    Chromatic, 
    i, ii, iii, iv, v, vi, vii,
    diatones,
    dflat, flat, nat, sharp, dsharp,
) where

import Suihei.Data.Modular (Mod)

-- | Represents the pitches of a diatonic scale.
type Diatone = Int `Mod` 7
type Semitone = Int `Mod` 12

-- | Represents musical objects that are diatonic in nature.
type Diatonic a = (Diatone, a)

-- | Represents (in combination with 'Diatone') the pitches of a chromatic scale.
type Accidental = Int

type Enharmonic = Diatonic Accidental

-- | Represents musical objects that are chromatic in nature.
type Chromatic a = (Enharmonic, a)

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

dflat, flat, nat, sharp, dsharp :: Accidental
dflat  = -2
flat   = -1
nat    = 0
sharp  = 1
dsharp = 2