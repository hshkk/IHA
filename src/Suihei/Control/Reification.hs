{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Suihei.Control.Reification (
    reify
) where

import Suihei.Control.Prelude (
    octaviate,
    triad,
    rotatedBy, scale, Trans (trans))
import Suihei.Data.Chromatic (Enharmonic, Accidental, diaFrom)
import Suihei.Data.Chordal (
    Key, base,
    pattern Analysis, Analysis (Shift), Variation (..),
    TAnalysis)
import Suihei.Data.Notal (Note)
import Suihei.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Progression, foldP,
    Music)

import Control.Monad.Trans.State (
    StateT,
    get, modify, evalStateT)

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.Extra (snoc, sortBy, foldl1')
import Data.List (delete, find)
import Data.Ord (comparing)

type State a = StateT [Key] Identity a

reify :: Progression -> Music
reify p = let -- As progressions can only consist of serial compositions, they can be converted to a list of units.
              ps = foldP return (++) p
              -- Then, each analysis can be converted to a parallel composition to be folded over.
              ms = runIdentity $ evalStateT (mapM reify' ps) [base]
          in foldl1' (:>) ms
    where reify' :: TAnalysis -> State Music
          reify' (Analysis key vs, dur) = do
              shifts <- get
              let key'  = foldl1' trans (key : shifts)
              let units = map (Unit . (, dur) . Just) (notes key' vs)
              return (foldl1' (:^) units)
          reify' (Shift key ana, dur) = modify (`snoc` key) >> reify' (ana, dur)

-- | Given a key and a set of variations, generates a set of constituent notes.
notes :: Key -> [Variation] -> [Note]
notes key []            = octaviate_ (triad key)
notes key (Snd key':vs) = notes (trans key key') vs
notes key (Inv i:vs)    = octaviate_ $ map fst (notes key vs `rotatedBy` (i - 1))
notes key (Adj i acc:vs)
    | Just note <- find (\(enh, _) -> fst enh == diaFrom inKey) result
                 = sortBy comparator ((note `delete` result) `snoc` adjust_ acc inKey)
    | otherwise  = result
    where result = notes key vs
          inKey  = i `inKey_` key
notes key (Add i:vs)    = sortBy comparator (notes key vs `snoc` inKey)
    where inKey  = i `inKey_` key
notes key (Sub i:vs)
    | Just note <- find (\(enh, oct) -> fst enh == diaFrom inKey
                                     && oct     == snd inKey) result
                 = note `delete` result
    | otherwise  = result
    where result = notes key vs
          inKey  = second succ (i `inKey_` key)

comparator :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
comparator = comparing snd <> comparing fst

-- | Given an interval and a key, determines the note of that interval.
inKey_ :: Int -> Key -> Note
inKey_ i key = octaviate_ (scale key) !! (i - 1)

-- | Combines the associated accidental of a note with an input accidental.
adjust_ :: Accidental -> Note -> Note
adjust_ acc ((dia, acc'), oct) = ((dia, acc + acc'), oct)

-- | Chordal reification is accomplished with respect to the fourth octave.
octaviate_ :: [Enharmonic] -> [Note]
octaviate_ = octaviate 4