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
    = U_ a Duration
    | S_ (Sequence a) (Sequence a)
    | P_ (Sequence a) (Sequence a)

class Serial_ a
class Parallel_ a

instance Serial_ Analysis

instance Serial_ Note
instance Parallel_ Note

{-# COMPLETE Unit, (:>) :: Progression #-}
{-# COMPLETE Unit, (:>), (:^) :: Music #-}

pattern Unit :: a -> Duration -> Sequence a
pattern Unit a d = U_ a d

pattern (:>) :: Serial_ a => Sequence a -> Sequence a -> Sequence a
pattern (:>) s1 s2 = S_ s1 s2
infixr 6 :>

pattern (:^) :: Parallel_ a => Sequence a -> Sequence a -> Sequence a
pattern (:^) s1 s2 = P_ s1 s2
infixr 5 :^

type Progression = Sequence Analysis

type Music = Sequence Note