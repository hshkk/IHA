{-# LANGUAGE FlexibleInstances #-}

module Calla.Data.Notal (
    Note,
    Music
) where

import Calla.Data.Chromatic (Chromatic (..))
import Calla.Data.Temporal (Contrapuntal)

type Note   = Chromatic Octave
type Octave = Int

type Music = Contrapuntal Note