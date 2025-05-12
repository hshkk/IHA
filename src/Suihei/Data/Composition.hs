{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Suihei.Data.Composition (
    Total (..),
    Take  (..),
    pattern Unit, pattern (:>), pattern (:^),
    Progression, foldP,
    Music,       foldM
) where

import Suihei.Data.Chordal (Analysis, TAnalysis)
import Suihei.Data.Notal (Note, TNote, pattern Rest)
import Suihei.Data.Temporal (Duration)

data Composition a
    = U_ a
    | S_ (Composition a) (Composition a)
    | P_ (Composition a) (Composition a)
    deriving Functor

foldC :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Composition a -> b
foldC f _ _ (U_ x)     = f x
foldC f g h (S_ x1 x2) = foldC f g h x1 `g` foldC f g h x2
foldC f g h (P_ x1 x2) = foldC f g h x1 `h` foldC f g h x2

class Total a where
    duration :: a -> Duration
    empty    :: Duration -> a

instance Total a => Total (Composition a) where
    duration = foldC duration (+) max
    empty    = U_ . empty

instance Total TNote where
    duration (_, d) = d
    empty           = (,) Rest

instance Total TAnalysis where
    duration (_, d) = d
    empty           = (,) undefined

class Take a where
    takenFrom   :: Duration -> a -> a
    droppedFrom :: Duration -> a -> a

instance (Take a, Total a) => Take (Composition a) where
    takenFrom d _ | d <= 0 = empty 0
    takenFrom d (U_ x) = U_ (d `takenFrom` x)
    takenFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then d `takenFrom` s1
                   else S_ s1 ((d - d1) `takenFrom` s2)
    takenFrom d (P_ s1 s2) = P_ (d `takenFrom` s1) (d `takenFrom` s2)

    droppedFrom d s | d <= 0 = s
    droppedFrom d (U_ x) = U_ (d `takenFrom` x)
    droppedFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then S_ (d `droppedFrom` s1) s2
                   else (d - d1) `droppedFrom` s2
    droppedFrom d (P_ s1 s2) = P_ (d `droppedFrom` s1) (d `droppedFrom` s2)

instance Take TNote where
    takenFrom   d1 (n, d2) = (n, min d1 d2)
    droppedFrom d1 (n, d2) = (n, max 0 (d2 - d1))

instance Take TAnalysis where
    takenFrom   d1 (n, d2) = (n, min d1 d2)
    droppedFrom d1 (n, d2) = (n, max 0 (d2 - d1))

class Serial_ a
class Parallel_ a

instance Serial_ Analysis

instance Serial_ TAnalysis

instance Serial_ Note
instance Parallel_ Note

instance Serial_ TNote
instance Parallel_ TNote

pattern Unit :: a -> Composition a
pattern Unit a = U_ a

pattern (:>) :: Serial_ a => Composition a -> Composition a -> Composition a
pattern (:>) s1 s2 = S_ s1 s2
infixl 5 :>

pattern (:^) :: Parallel_ a => Composition a -> Composition a -> Composition a
pattern (:^) s1 s2 = P_ s1 s2
infixl 6 :^

type Progression = Composition TAnalysis

foldP :: (TAnalysis -> a) -> (a -> a -> a) -> Progression -> a
foldP f g = foldC f g undefined

type Music = Composition TNote

foldM :: (TNote -> a) -> (a -> a -> a) -> (a -> a -> a) -> Music -> a
foldM = foldC

{-# COMPLETE Unit, (:>) :: Progression #-}
{-# COMPLETE Unit, (:>), (:^) :: Music #-}