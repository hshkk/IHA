{-# LANGUAGE PatternSynonyms #-}

module IHA.Control.Inference (
    infer
) where

import IHA.Control.Demarcation (demarcate)
import IHA.Control.Template (label)
import IHA.Control.Prelude (Trans (trans), inverse)
import IHA.Data.Chordal (Key)
import IHA.Data.Composition (
    Progression, Music,
    pattern Unit, pattern (:>))

import IHA.Text.Prettyprinter ()

import Data.List (foldl1')

infer :: Music -> Key -> Progression
infer m key = let m' = trans m (inverse key)
                  ts = map (snd . label) (demarcate m')
              in foldl1' (:>) (map Unit ts)