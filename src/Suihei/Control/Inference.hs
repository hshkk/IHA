{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Inference (
    infer
) where

import Suihei.Control.Demarcation (demarcate)
import Suihei.Control.Template (label)
import Suihei.Control.Prelude (Trans (trans), inverse)
import Suihei.Data.Chordal (Key)
import Suihei.Data.Composition (
    Progression, Music,
    pattern Unit, pattern (:>))

import Suihei.Text.Prettyprinter ()

import Data.List (foldl1')

infer :: Music -> Key -> Progression
infer m key = let m' = trans m (inverse key)
                  ts = map (snd . label) (demarcate m') 
              in foldl1' (:>) (map Unit ts)