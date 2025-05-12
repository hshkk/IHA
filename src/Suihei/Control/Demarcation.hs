{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Demarcation (
    demarcate, partition
) where

import Suihei.Data.Composition (
    duration, 
    Take (takenFrom, droppedFrom),
    pattern Unit, pattern (:>), pattern (:^),
    Music, foldM)

type Demarcation a = [a]

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
--  (2) [Proximity] For a sequence of notes n1 > n2 > n3 > n4, n2 > n3 could be a demarcation boundary if:
--      * The interval of time from the end of n2 is greater than that from the end of n1 to n2 and that from the end of n3 to n4.
--  (3) [Change] For a sequence of notes n1 > n2 > n3 > n4, n2 > n3 could be a demarcation boundary if:
--      * n2 > n3 involves a greater intervallic distance than both n1 > n2 and n3 > n4, or if
--      * n2 > n3 have multiple constituents, but have very few or no pitch classes in common.
--      * n2 and n3 are of different length and both n1, n2 and n3, n4 do not differ in length.
--  (4) [Intensification] The more pronounced the effects of (2) and (3) are, the more likely a demarcation boundary is.
--  (5) [Symmetry] Demarcations that approach the ideal subdivision of two parts of equal length are preferred over asymmetric ones.
demarcate :: Music -> Demarcation Music
demarcate = foldM (\n -> [Unit n]) 
                  (/+/)
                  (unmarcate (:^) (:>)) . partition
    where -- Unmarcates or constructs a demarcation by concatenation.
          -- (++) is equivalent to the most eager demarcation algorithm: every serial composition is rewritten as a demarcation.
          -- @unmarcate@ is equivalent to the most hesitant demarcation algorithm: every serial composition remains intact.
          (/+/) :: Demarcation Music -> Demarcation Music -> Demarcation Music
          d1 /+/ d2 = [] -- TODO: Implement!
          infix 9 /+/

-- | "Unmarcates" (i.e. merges) two demarcations by folding over their constituents, yielding a singular demarcation.
unmarcate :: (a -> a -> a) -> (a -> a -> a) -> Demarcation a -> Demarcation a -> Demarcation a
unmarcate f g d1 d2 = [unify f g d1 d2]

-- | Unifies two demarcations by folding over their constituents, yielding a singular value.
-- The third and fourth arguments, @d1@ and @d2@, are each folded with the second argument @g@, then applied together to the first argument @f@.
unify :: (a -> a -> a) -> (a -> a -> a) -> Demarcation a -> Demarcation a -> a
unify f g d1 d2 = foldr1 g d1 `f` foldr1 g d2

-- | Resolves serial compositions nested in parallel compositions to avoid demarcations over sustained notes.
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
    where -- Partitions serial compositions nested in parallel compositions via segmentation of the latter.
          -- If two serial compositions occur simultaneously, all four arguments are recursively processed.
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
          segment :: Music -> Music -> Music -> Music -> Music
          segment m1a m1b m2a m2b
              | d1 > d2    = (d2 `takenFrom` m1a) // m2a :> (d2 `droppedFrom` m1a) // m2b :> m1b // m2b
              | d1 < d2    = m1a // (d1 `takenFrom` m2a) :> m1b // (d1 `droppedFrom` m2a) :> m1b // m2b
              | otherwise  = m1a // m2a :> m1b // m2b
                  where d1 = duration m1a
                        d2 = duration m2a