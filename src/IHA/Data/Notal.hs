{-# LANGUAGE PatternSynonyms #-}

module IHA.Data.Notal (
    Octave, 
    Note, pattern Rest,
    TNote, INote (..)
) where

import IHA.Data.Chromatic (Chromatic)
import IHA.Data.Temporal (Temporal)

import GHC.Natural (Natural)

type Octave = Natural
type Note   = Chromatic Octave

pattern Rest :: Maybe Note
pattern Rest = Nothing

type TNote = Temporal (Maybe Note)
-- | @INote@ only exists to pretty-print notes using absolute names.
-- In a future iteration, this system should be rewritten.
newtype INote = In TNote