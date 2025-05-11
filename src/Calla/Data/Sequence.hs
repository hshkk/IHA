{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Sequence (
    Duration,
    duration,
    pattern Unit, pattern (:>), pattern (:^),
    Progression, foldP,
    Music,       foldM
) where

import Data.Ratio (Ratio)

import Calla.Data.Chordal (Analysis)
import Calla.Data.Notal (Note)

type Duration = Ratio Int

data Sequence a
    = U_ a Duration
    | S_ (Sequence a) (Sequence a)
    | P_ (Sequence a) (Sequence a)
    deriving Functor

foldS :: (a -> Duration -> b) -> (b -> b -> b) -> (b -> b -> b) -> Sequence a -> b
foldS f _ _ (U_ x d)   = f x d
foldS f g h (S_ x1 x2) = foldS f g h x1 `g` foldS f g h x2
foldS f g h (P_ x1 x2) = foldS f g h x1 `h` foldS f g h x2

duration :: Sequence a -> Duration
duration = foldS (const id) (+) max

class Serial_ a
class Parallel_ a

instance Serial_ Analysis

instance Serial_ Note
instance Parallel_ Note

instance Serial_ (Maybe Note)
instance Parallel_ (Maybe Note)

pattern Unit :: a -> Duration -> Sequence a
pattern Unit a d = U_ a d

pattern (:>) :: Serial_ a => Sequence a -> Sequence a -> Sequence a
pattern (:>) s1 s2 = S_ s1 s2
infixl 5 :>

pattern (:^) :: Parallel_ a => Sequence a -> Sequence a -> Sequence a
pattern (:^) s1 s2 = P_ s1 s2
infixl 6 :^

type Progression = Sequence Analysis

foldP :: (Analysis -> Duration -> a) -> (a -> a -> a) -> Progression -> a
foldP f g = foldS f g undefined

type Music = Sequence (Maybe Note)

foldM :: (Maybe Note -> Duration -> a) -> (a -> a -> a) -> (a -> a -> a) -> Music -> a
foldM = foldS

{-# COMPLETE Unit, (:>) :: Progression #-}
{-# COMPLETE Unit, (:>), (:^) :: Music #-}