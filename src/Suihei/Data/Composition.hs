{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Suihei.Data.Composition (
    Composition,
    Serial, Parallel,
    pattern Unit, pattern (:>), pattern (:^),
    Progression, foldP,
    Music,       foldM,
    Total (..), Take  (..),
    fore, aft, count, trim,
) where

import Suihei.Data.Chordal (Analysis, TAnalysis, IAnalysis)
import Suihei.Data.Notal (Note, TNote, pattern Rest, INote)
import Suihei.Data.Temporal (Duration, Temporal)

data Composition a
    = U_ !a
    | S_ !(Composition a) !(Composition a)
    | P_ !(Composition a) !(Composition a)
    deriving Eq

instance Functor Composition where
    fmap f (U_ x) = U_ (f x)
    fmap f (S_ x1 x2) = S_ (fmap f x1) (fmap f x2)
    fmap f (P_ x1 x2) = P_ (fmap f x1) (fmap f x2)

foldC :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Composition a -> b
foldC f _ _ (U_ x)     = f $! x
foldC f g h (S_ x1 x2) = g (foldC f g h x1) $! foldC f g h x2
foldC f g h (P_ x1 x2) = h (foldC f g h x1) $! foldC f g h x2

class Serial a
class Parallel a

instance Serial Analysis

instance Serial TAnalysis

instance Serial IAnalysis

instance Serial Note
instance Parallel Note

instance Serial TNote
instance Parallel TNote

instance Serial INote
instance Parallel INote

pattern Unit :: a -> Composition a
pattern Unit a = U_ a

pattern (:>) :: Serial a => Composition a -> Composition a -> Composition a
pattern (:>) x1 x2 = S_ x1 x2
infixl 5 :>

pattern (:^) :: Parallel a => Composition a -> Composition a -> Composition a
pattern (:^) x1 x2 = P_ x1 x2
infixl 6 :^

type Progression = Composition TAnalysis

foldP :: (TAnalysis -> a) -> (a -> a -> a) -> Progression -> a
foldP f g = foldC f g undefined

type Music = Composition TNote

foldM :: (TNote -> a) -> (a -> a -> a) -> (a -> a -> a) -> Music -> a
foldM = foldC

{-# COMPLETE Unit, (:>) :: Progression #-}
{-# COMPLETE Unit, (:>), (:^) :: Music #-}

class Total a where
    duration :: a -> Duration
    empty    :: Duration -> a

instance Total a => Total (Composition a) where
    duration = foldC duration (+) max
    empty    = U_ . empty

instance Total TNote where
    duration (_, dur) = dur
    empty             = (,) Rest

instance Total TAnalysis where
    duration (_, dur) = dur
    empty             = (,) undefined

class Take a where
    takenFrom   :: Duration -> a -> a
    droppedFrom :: Duration -> a -> a

instance (Take a, Total a) => Take (Composition a) where
    takenFrom dur _ | dur <= 0 = empty 0
    takenFrom dur (U_ x)       = U_ (dur `takenFrom` x)
    takenFrom dur (S_ x1 x2)   = let dur' = duration x1 in
        if dur <= dur' then dur `takenFrom` x1
                   else S_ x1 ((dur - dur') `takenFrom` x2)
    takenFrom dur (P_ x1 x2)   = P_ (dur `takenFrom` x1) (dur `takenFrom` x2)

    droppedFrom dur s | dur <= 0 = s
    droppedFrom dur (U_ x)       = U_ (dur `droppedFrom` x)
    droppedFrom dur (S_ x1 x2)   = let dur' = duration x1 in
        if dur <= dur' then S_ (dur `droppedFrom` x1) x2
                   else (dur - dur') `droppedFrom` x2
    droppedFrom dur (P_ x1 x2)   = P_ (dur `droppedFrom` x1) (dur `droppedFrom` x2)

instance Take TNote where
    takenFrom   dur1 (n, dur2) = (n, min dur1 dur2)
    droppedFrom dur1 (n, dur2) = (n, max 0 (dur2 - dur1))

instance Take TAnalysis where
    takenFrom   dur1 (n, dur2) = (n, min dur1 dur2)
    droppedFrom dur1 (n, dur2) = (n, max 0 (dur2 - dur1))

-- | Retrieves the first element of a composition with respect to the outermost serial composition.
fore :: Composition a -> Composition a
fore u@(U_ _)   = u
fore (S_ x _)   = fore x
fore p@(P_ x1 x2)
    | S_ _ _ <- x1
    , S_ _ _ <- x2 = p
    | S_ _ _ <- x1 = P_ x1 (fore x2)
    | S_ _ _ <- x2 = P_ (fore x1) x2
    | otherwise    = P_ (fore x1) (fore x2)

-- | Retrieves the last element of a composition with respect to the outermost serial composition.
aft :: Composition a -> Composition a
aft = fore . rev
    where rev :: Composition a -> Composition a
          rev = foldC U_ (flip S_) P_

-- | Counts the number of elements in a composition with respect to the outermost serial compositions.
count :: Composition a -> Int
count (S_ x1 x2) = count x1 + count x2
count _          = 1

-- | Removes all elements in a composition with null durations.
-- @trim@ is often used in combination with @takenFrom@ or @droppedFrom@.
trim :: Composition (Temporal a) -> Maybe (Composition (Temporal a))
trim u@(U_ (_, dur))
    | dur == 0  = Nothing
    | otherwise = Just u
trim s@(S_ x1 x2) = case (trim x1, trim x2) of
    (x1', Nothing)   -> x1'
    (Nothing, x2')   -> x2'
    (Just _, Just _) -> Just s
trim p@(P_ x1 x2) = case (trim x1, trim x2) of
    (x1', Nothing)   -> x1'
    (Nothing, x2')   -> x2'
    (Just _, Just _) -> Just p