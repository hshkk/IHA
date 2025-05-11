{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Suihei.Data.Sequence (
    Duration, duration,
    Take (..),
    pattern Unit, pattern (:>), pattern (:^),
    Progression, foldP,
    Music,       foldM
) where

import Suihei.Data.Chordal (Analysis)
import Suihei.Data.Notal (Note)

import Data.Ratio (Ratio)

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

class Take a where
    takenFrom   :: Duration -> a -> a
    droppedFrom :: Duration -> a -> a

instance {-# OVERLAPPING #-} Take (Sequence (Maybe a)) where
    takenFrom d _ | d <= 0 = U_ Nothing 0
    takenFrom d1 (U_ x d2) = U_ x (min d1 d2)
    takenFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then d `takenFrom` s1
                   else S_ s1 ((d - d1) `takenFrom` s2)
    takenFrom d (P_ s1 s2) = P_ (d `takenFrom` s1) (d `takenFrom` s2)

    droppedFrom d s | d <= 0 = s
    droppedFrom d1 (U_ x d2) = U_ x (max 0 (d2 - d1))
    droppedFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then S_ (d `droppedFrom` s1) s2
                   else (d - d1) `droppedFrom` s2
    droppedFrom d (P_ s1 s2) = P_ (d `droppedFrom` s1) (d `droppedFrom` s2)

instance {-# OVERLAPPABLE #-} Take (Sequence a) where
    takenFrom d _ | d <= 0 = undefined
    takenFrom d1 (U_ x d2) = U_ x (min d1 d2)
    takenFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then d `takenFrom` s1
                   else S_ s1 ((d - d1) `takenFrom` s2)
    takenFrom d (P_ s1 s2) = P_ (d `takenFrom` s1) (d `takenFrom` s2)

    droppedFrom d s | d <= 0 = s
    droppedFrom d1 (U_ x d2) = U_ x (max 0 (d2 - d1))
    droppedFrom d (S_ s1 s2) = let d1 = duration s1 in
        if d <= d1 then S_ (d `droppedFrom` s1) s2
                   else (d - d1) `droppedFrom` s2
    droppedFrom d (P_ s1 s2) = P_ (d `droppedFrom` s1) (d `droppedFrom` s2)

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