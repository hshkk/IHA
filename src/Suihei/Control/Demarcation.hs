{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Demarcation (
    demarcate,
    partition
) where

import Suihei.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Music, foldM,
    Total (duration),
    Take  (takenFrom, droppedFrom))
import Suihei.Text.Prettyprinter ()

import Data.List.Extra (uncons, unsnoc, foldl')
import Suihei.Control.Template (label)

type Demarcation a = [a]

-- | Demarcates (i.e. segments) music into harmonically "significant" partitions.
demarcate :: Music -> Demarcation Music
demarcate m = let mins :: Demarcation Music -- Partitions music so as to remove nested serial compositions.
                  mins = foldM (return . Unit) (++) (unmarcate (:^)) (partition m)
              in foldl' (/+/) [Unit (Nothing, 0)] mins
    where -- Unmarcates or constructs a demarcation by concatenation.
          -- (++) is equivalent to the most eager demarcation algorithm: every serial composition is rewritten as a demarcation.
          -- @unmarcate@ is equivalent to the most hesitant demarcation algorithm: every serial composition remains intact.
          (/+/) :: Demarcation Music -> Music -> Demarcation Music
          d1 /+/ m2
              -- Due to pre-partitioning, m1 (the last demarcation) will have no serial compositions.
              | Just (_, m1) <- unsnoc d1 =
                  let s12 = fst $! label (m1 :> m2)
                      s1  = fst $! label m1
                      s2  = fst $! label m2 in
                  if s12 >= s1 + s2            -- Determines which score matches stronger to a template.
                  then unmarcate (:>) d1 [m2]  -- Merges if the score of m1 :> m2 is greater.
                  else d1 ++ [m2]              -- Creates a demarcation if the score of m2 is greater.
              | otherwise = []
          infix 8 /+/

-- | "Unmarcates" (i.e. merges) two demarcations by composing their last and first elements with a given function.
unmarcate :: (a -> a -> a) -> Demarcation a -> Demarcation a -> Demarcation a
unmarcate f d1 d2
    | Just (m1s, m1) <- unsnoc d1
    , Just (m2, m2s) <- uncons d2 = concat [m1s, [m1 `f` m2], m2s]
    | otherwise = d1 ++ d2

-- | Partitions serial compositions nested within parallel compositions to avoid demarcations over sustained notes.
--
-- >>> C4 1/4 ^ (E4 1/8 > G4 1/8)
-- (C4 1/8 ^ E4 1/8) > (C4 1/8 ^ G4 1/8)
--
-- >>> (C4 1/4 > G4 1/4) ^ (C5 1/6 > E5 1/6 > G5 1/6)
-- C4 1/6 ^ C5 1/6 > C4 1/12 ^ E5 1/12 > G4 1/12 ^ E5 1/12 > G4 1/6 ^ G5 1/6
partition :: Music -> Music
partition = foldM Unit
                  (\m1 m2 -> partition m1 :> partition m2)
                  (//)
    where -- Recursively partitions serial compositions nested within parallel compositions via segmentation of the latter.
          -- If two serial compositions occur simultaneously, all four arguments are segmented accordingly.
          (//) :: Music -> Music -> Music
          m1 // m2
              | m1a :> m1b <- m1
              , m2a :> m2b <- m2 = segment m1a m1b m2a m2b
              | m1a :> m1b <- m1 = m1a // m2  :> m1b // m2
              | m1a :> m1b <- m2 = m1  // m1a :> m1  // m1b
              | otherwise        = let d = min (duration m1) (duration m2) in
                                       d `takenFrom` m1 :^ d `takenFrom` m2
          infix 8 //
          -- Segments two simultaneous serial compositions, effectively rewriting all simultaneous serial compositions.
          --
          -- (d 1/2 > e 1/2) ^ (a 1/3 > b 1/3 > c 1/3):
          --    +-----+-----+-----+
          -- 1> |  a  |  b  |  c  |
          --    +-----+--+--+-----+
          -- 2> |  d  :  |  :  e  |
          --    +-----+--+--+-----+
          -- d and e must be split accordingly with b.
          -- The duration of d is greater than that of a, so the first guard applies.
          -- 
          -- (c 1/3 > d 1/3 > e 1/3) ^ (a 1/2 > b 1/2):
          --    +-----+--+--+-----+
          -- 1> |   a :  |  : b   |
          --    +-----+--+--+-----+
          -- 2> |  c  |  d  |  e  |
          --    +-----+-----+-----+
          -- a and b must be split accordingly with d.
          -- The duration of c is less than that of a, so the second guard applies.
          segment :: Music -> Music -> Music -> Music -> Music
          segment m1a m1b m2a m2b
              | d1 > d2    = d2 `takenFrom` m1a // m2a :> d2 `droppedFrom` m1a // m2b :> m1b // m2b
              | d1 < d2    = m1a // d1 `takenFrom` m2a :> m1b // d1 `droppedFrom` m2a :> m1b // m2b
              | otherwise  = m1a // m2a :> m1b // m2b
                  where d1 = duration m1a
                        d2 = duration m2a