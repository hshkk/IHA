{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Suihei.Data.Chromatic (
    Diatone, Semitone,
    Diatonic,
    Accidental,
    Enharmonic,
    Chromatic, 
    diaFrom, accFrom,
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

diaFrom :: Chromatic a -> Diatone
diaFrom = fst . fst

accFrom :: Chromatic a -> Accidental
accFrom = snd . fst

diatones :: [Diatone]
diatones = [0..6]

dflat, flat, nat, sharp, dsharp :: Accidental
dflat  = -2
flat   = -1
nat    = 0
sharp  = 1
dsharp = 2