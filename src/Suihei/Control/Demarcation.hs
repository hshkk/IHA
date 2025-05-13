{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Demarcation (
    demarcate
) where

import Suihei.Control.Prelude (pitchClasses)
import Suihei.Data.Composition (
    fore, aft, count,
    pattern Unit, pattern (:>), pattern (:^),
    Music, foldM,
    Total (duration),
    Take  (takenFrom, droppedFrom))
import Suihei.Data.Probability (Probability)

import Data.List.Extra (uncons, unsnoc)

type Demarcation a = [a]

-- | Retrieves the first element from the first composition within a demarcation.
initial :: Demarcation Music -> Maybe Music
initial d = fore . fst <$> uncons d

-- | Retrieves the last element from the last composition within a demarcation.
terminal :: Demarcation Music -> Maybe Music
terminal d = aft . snd <$> unsnoc d

-- | Demarcates (i.e. segments) music into harmonically "significant" partitions.
-- 
-- Demarcations are generated with rules heavily inspired by the GPRs of the generative theory of tonal music (GTTM).
-- However, demarcations are not hierarchic, unlike the grouping model proposed by GTTM.
-- Therefore, demarcations obey only the following well-formedness rules.
-- 
--  * Any contiguous sequence of notes can constitute a demarcation, and only contiguous sequences may constitute a demarcation.
--  * Any piece can be demarcated, with a lower bound of a single demarcation (i.e. the entire piece).
--
-- Finally, demarcations obey the following preference rules.
--
--  (1) [Size] The smaller a demarcation, the less preferable it is.
--  (2) [Change] For a sequence of notes n1 > n2 > n3 > n4, n2 > n3 could be a demarcation boundary if:
--      * n2 > n3 involves a greater intervallic distance than both n1 > n2 and n3 > n4, or if
--      * n2 > n3 have multiple constituents, but have very few or no pitch classes in common.
--      * n2 and n3 are of different length and both n1, n2 and n3, n4 do not differ in length.
--  (3) [Intensification] The more pronounced the effects of (2) and (3) are, the more likely a demarcation boundary is.
--  (4) [Symmetry] Demarcations that approach the ideal subdivision of two parts of equal length are preferred over asymmetric ones.
demarcate :: Music -> Demarcation Music
demarcate = foldM (\n -> [Unit n])
                  (/+/)
                  (unmarcate (:^) (:>)) . partition
    where -- Unmarcates or constructs a demarcation by concatenation.
          -- (++) is equivalent to the most eager demarcation algorithm: every serial composition is rewritten as a demarcation.
          -- @unmarcate@ is equivalent to the most hesitant demarcation algorithm: every serial composition remains intact.
          (/+/) :: Demarcation Music -> Demarcation Music -> Demarcation Music
          d1 /+/ d2
              -- Due to pre-partitioning, m1 and m2 will have no serial compositions.
              | Just m1 <- terminal d1
              , Just m2 <- initial d2 =
                  let m1p = pitchClasses m1
                      m2p = pitchClasses m2 in []
              | otherwise = []
          infix 9 /+/

-- | "Unmarcates" (i.e. merges) two demarcations by folding over their constituents, yielding a singular demarcation.
-- The third and fourth arguments, @d1@ and @d2@, are each folded with the second argument @g@, then applied together to the first argument @f@.
unmarcate :: (a -> a -> a) -> (a -> a -> a) -> Demarcation a -> Demarcation a -> Demarcation a
unmarcate f g d1 d2 = [foldr1 g d1 `f` foldr1 g d2]

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
          infix 9 //
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
              | d1 > d2    = (d2 `takenFrom` m1a) // m2a :> (d2 `droppedFrom` m1a) // m2b :> m1b // m2b
              | d1 < d2    = m1a // (d1 `takenFrom` m2a) :> m1b // (d1 `droppedFrom` m2a) :> m1b // m2b
              | otherwise  = m1a // m2a :> m1b // m2b
                  where d1 = duration m1a
                        d2 = duration m2a

size :: Demarcation Music -> Demarcation Music -> Probability
size d1 d2 = 0
    where l1 = count <$> terminal d1
          l2 = count <$> initial d2