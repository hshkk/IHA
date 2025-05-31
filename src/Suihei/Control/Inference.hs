{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Inference (
    infer
) where

import Suihei.Control.Demarcation (demarcate)
import Suihei.Control.Template (label)
import Suihei.Control.Prelude (Trans(trans), finite)
import Suihei.Data.Chordal (Key)
import Suihei.Data.Composition (
    Progression, Music,
    pattern (:>), trim)
import Suihei.Data.Modular (toMod)

import Data.List (find, elemIndex, foldl1')
import Data.Maybe (fromJust, mapMaybe)

infer :: Music -> Key -> Progression
infer m key = let m' = trans m (inverse key)
              in foldl1' (:>) (map (snd . label) (mapMaybe trim (demarcate m')))
    where -- Determines the transposition required to normalize all notes down to I.
          inverse :: Key -> Key
          inverse key' = let enh  = fromJust (find ((0 ==) . fst) (finite key'))
                             dia  = toMod (fromJust (enh `elemIndex` finite key'))
                             acc  = negate (snd enh)
                         in ((dia, acc), snd key')