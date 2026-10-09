{-# LANGUAGE PatternSynonyms #-}

module IHA.Control.Demarcation (
    demarcate
)  where

import IHA.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Music, foldM,
    Total (duration),
    Take  (takenFrom, droppedFrom))

import Data.Function (on)
import Data.List (foldl')
import Data.List.Extra (uncons, unsnoc, snoc)
import IHA.Control.Template (label)

-- | Demarcates music into harmonically significant segments.
demarcate :: Music -> [Music]
demarcate m = let mins = foldM (return . Unit) (++) (unmarcate (:^)) $ partition m in
              foldl' (/+/) [] mins
    where (/+/) :: [Music] -> Music -> [Music]
          d /+/ m2
              | Just (_, m1) <- unsnoc d =
                  let com = fst (label (m1 :> m2))
                      s1  = fst (label m1)
                      s2  = fst (label m2)
                  in if com >= s1 + s2
                     then unmarcate (:>) d [m2]
                     else d `snoc` m2
              | otherwise                = [m2]

-- | "Unmarcates" (i.e. merges) two demarcations by composing their last and first elements within a given function.
unmarcate :: (a -> a -> a) -> [a] -> [a] -> [a]
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
                  ((:>) `on` partition)
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
              | d1 > d2    = d2 `takenFrom` m1a // m2a 
                          :> d2 `droppedFrom` m1a // m2b 
                          :> m1b // m2b
              | d1 < d2    = m1a // d1 `takenFrom` m2a 
                          :> m1b // d1 `droppedFrom` m2a 
                          :> m1b // m2b
              | otherwise  = m1a // m2a :> m1b // m2b
                  where d1 = duration m1a
                        d2 = duration m2a