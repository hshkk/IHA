{-# LANGUAGE PatternSynonyms #-}

module Calla.Control.Demarcation (
    demarcate
) where

import Calla.Data.Sequence (
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
demarcate = foldM (\n d -> [Unit n d])
                  const
                  const