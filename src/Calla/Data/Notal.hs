{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Notal (
    Octave, 
    Note, pattern Rest
) where

import Calla.Data.Chromatic (Chromatic)

type Octave = Int
-- | Notes are considered in absolute terms. 
-- In other words, the associated `Diatone` is with respect to C as 0.
type Note   = Chromatic Octave

pattern Rest :: Maybe Note
pattern Rest = Nothing