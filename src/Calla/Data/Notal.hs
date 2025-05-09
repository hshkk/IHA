{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Calla.Data.Notal (
    Octave, Note
) where

import Calla.Data.Chromatic (Chromatic)

type Octave = Int
type Note   = Chromatic Octave