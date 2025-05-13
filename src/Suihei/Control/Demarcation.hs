{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Demarcation (
    demarcate
) where

import Suihei.Control.Prelude (
    pitchClasses, 
    enharmonic, 
    occurrences, 
    notes, triad)
import Suihei.Data.Chromatic (Enharmonic, Semitone)
import Suihei.Data.Chordal (Mode (Maj, Min))
import Suihei.Data.Notal (Octave, Note)
import Suihei.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Music, foldM,
    Total (duration),
    Take  (takenFrom, droppedFrom),
    fore, aft)
import Suihei.Data.Temporal (Duration)
import Suihei.Data.Probability (Probability)

import Data.List (nub, intersect, union, sort)
import Data.List.Extra (uncons, unsnoc)
import Data.Tuple (swap)

type Demarcation a = [a]

-- | Retrieves the first element from the first composition within a demarcation.
initial :: Demarcation Music -> Maybe Music
initial d = fore . fst <$> uncons d

-- | Retrieves the last element from the last composition within a demarcation.
terminal :: Demarcation Music -> Maybe Music
terminal d = aft . snd <$> unsnoc d

durationOver :: Demarcation Music -> Duration
durationOver = foldr ((+) . duration) 0

occurrencesOver :: Demarcation Music -> [(Enharmonic, Int)]
occurrencesOver = concatMap occurrences 

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
--  (2) [Movement] For a sequence of notes n1 > n2, n1 > n2 could be a demarcation boundary if n1 and n2 have very few or no pitch classes in common.
--  (3) [Density] For a sequence of notes n1 > n2, n1 > n2 could be a demarcation boundary if n2 has more constituents than n1.
--  (4) [Identification] For a sequence of notes n1 > n2, n1 > n2 could be a demarcation boundary if the constituent pitch classes switch modes.
--  (5) [Intensification] The more pronounced the effects of (2) and (3) are, the more likely a demarcation boundary is.
--  (6) [Symmetry] Demarcations that approach the ideal subdivision of two parts of equal length are preferred over asymmetric ones.
demarcate :: Music -> Demarcation Music
demarcate = foldM (\n -> [Unit n])
                  (/+/)
                  (unmarcate (:^)) . partition
    where -- Unmarcates or constructs a demarcation by concatenation.
          -- (++) is equivalent to the most eager demarcation algorithm: every serial composition is rewritten as a demarcation.
          -- @unmarcate@ is equivalent to the most hesitant demarcation algorithm: every serial composition remains intact.
          (/+/) :: Demarcation Music -> Demarcation Music -> Demarcation Music
          d1 /+/ d2
              -- Due to pre-partitioning, m1 and m2 will have no serial compositions.
              | Just m1 <- terminal d1
              , Just m2 <- initial d2 =
                  let m1p = pitchClasses m1
                      m2p = pitchClasses m2
                      -- These probabilities, including the associated formula, is arbitrary.
                      -- A full rewrite is in order!
                      lp   = min ((fromIntegral . length) m1p) ((fromIntegral .length) m2p)
                      p1  = symm d1  d2
                      p2  = mvt  m1p m2p
                      p3  = dens m1p m2p
                      p4  = iden (notes m1) (notes m2) in
                  if (p1 + p2 * (lp - 1) + p3 + p4 * lp) / (2 + (lp - 1) + lp) > 0.4
                  then d1 ++ d2
                  else unmarcate (:>) d1 d2
              | otherwise = []
          infix 9 /+/

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

symm :: Demarcation Music -> Demarcation Music -> Probability
symm d1 d2 = 1 - realToFrac (abs (ds1 - ds2) / max ds1 ds2)
    where ds1 = durationOver d1
          ds2 = durationOver d2

-- | The degree of pitch class movement is determined by a Jaccard indexing over lists of pitch classes.
-- If there exists a discrepancy with the lowermost notes, the probability increases.
mvt :: [(Semitone, Octave)] -> [(Semitone, Octave)] -> Probability
mvt m1p m2p = let u = m1ps `union` m2ps
                  p | null u    = 0
                    | otherwise = 1 - fromIntegral (length (m1ps `intersect` m2ps))
                                    / fromIntegral (length (m1ps `union` m2ps)) in
    (case (m1l, m2l) of
        (Nothing, _) -> 1
        (_, Nothing) -> 1
        (_, _)       -> if m1l /= m2l then 1 else 0.75) * p
    where m1ps = nub (map fst m1p)
          m2ps = nub (map fst m2p)
          m1l  = fst <$> (uncons . sort . map swap) m1p
          m2l  = fst <$> (uncons . sort . map swap) m2p

dens :: [(Semitone, Octave)] -> [(Semitone, Octave)] -> Probability
dens m1p m2p = fromIntegral (abs (m1ps - m2ps)) / fromIntegral (max m1ps m2ps)
    where m1ps = length m1p
          m2ps = length m2p

iden :: [Note] -> [Note] -> Probability
iden m1p m2p = case m1l of
        Nothing          -> 0
        Just ((d, _), a) -> if major d a `subset` m1ps && minor d a `subset` m2ps
                            || minor d a `subset` m1ps && major d a `subset` m2ps then 1 else 0.5
    where m1ps = nub (map enharmonic m1p)
          m2ps = nub (map enharmonic m2p)
          m1l  = fst <$> (uncons . sort) m1p
          major d a = map (enharmonic . ($ undefined)) (triad ((d, Maj), a))
          minor d a = map (enharmonic . ($ undefined)) (triad ((d, Min), a))

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = xs `intersect` ys == xs