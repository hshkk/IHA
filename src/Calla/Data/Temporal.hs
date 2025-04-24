{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Temporal (
    Duration,
    Sequence,
    pattern Unit, pattern (:>), pattern (:^),
    Progression, Music
) where

import Data.Ratio (Ratio)

import Calla.Data.Chordal (Analysis)
import Calla.Data.Notal (Note)

type Duration = Ratio Int

data Sequence a
    = U a Duration
    | S (Sequence a) (Sequence a)
    | P (Sequence a) (Sequence a)

class Serial a
class Parallel a

instance Serial Analysis

instance Serial Note
instance Parallel Note

{-# COMPLETE Unit, (:>) :: Progression #-}
{-# COMPLETE Unit, (:>), (:^) :: Music #-}

pattern Unit :: a -> Duration -> Sequence a
pattern Unit a d = U a d

pattern (:>) :: Serial a => Sequence a -> Sequence a -> Sequence a
pattern (:>) s1 s2 = S s1 s2
infixr 6 :>

pattern (:^) :: Parallel a => Sequence a -> Sequence a -> Sequence a
pattern (:^) s1 s2 = P s1 s2
infixr 5 :^

type Progression = Sequence Analysis

type Music = Sequence Note