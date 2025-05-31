{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Template (
    label
) where

import Suihei.Control.Prelude (
    Semitonal (pitchClassOf),
    occurrences, rotatedBy, scale)
import Suihei.Control.Reification (reify)
import Suihei.Data.Chromatic (Semitone, flat, Accidental)
import Suihei.Data.Chordal (
    Mode (..),
    Analysis, pattern Analysis,
    Variation (..),
    pattern Dim, pattern Aug, base)
import Suihei.Data.Composition (
    Total (duration),
    pattern Unit,
    Music, Progression,
    foldM)
import Suihei.Data.Modular (toMod)

import Data.Bifunctor (first)
import Data.List (elemIndex, find, nub, sortBy)
import Data.Maybe (fromMaybe, fromJust, catMaybes, mapMaybe, isNothing)
import Data.Tuple (swap)

import Suihei.Text.Prettyprinter ()
import Debug.Trace (trace)
import Data.Ord (comparing)

type Template = [Semitone]

type Score = Int

type Weight = Int
type Vector = [(Semitone, Weight)]

type Root = Semitone

-- TODO: Currently, labeling only works relative to 1 (in relative notation) as the tonic.
-- To fix this issue, either 1) key inference, or 2) manual key input is necessary.
label :: Music -> (Score, Progression)
label m = if rests m
          then (minBound, Unit (Analysis base [], 0))
          else
            let highest = maximum (map (\p -> (score (weight m) p, p)) preference)
                sc      = fst (fst highest)
                likely  = first snd highest in
            (sc, Unit (invert (realize likely m) m, duration m))
    where preference = [ MinorSeventh, MajorSeventh, FullDiminished, HalfDiminished, DominantSeventh
                       , AugmentedTriad, DiminishedTriad, MinorTriad, MajorTriad
                       , Fifth, Tritone
                       , MinorThird, MajorThird
                       , Atom ]
          rests = foldM (isNothing . fst) (&&) (&&)

-- | Given the original music, and a progression, determines whether the progression should be inverted.
invert :: Analysis
       -> Music -- ^ Access to the original musical segment is required to generate the correct accidental.
       -> Analysis
invert ana@(Analysis key vs) m = if i == 1 
                                 then ana
                                 else Analysis key (Inv i : vs)
    where notes = nub $ catMaybes (foldM (return . fst) (++) (++) m)
          lowermost = (snd . minimum . map swap) notes
          constituents = map fst (mapMaybe fst $ foldM return undefined (++) (reify (Unit (ana, 0))))
          i = if lowermost `elem` constituents
              then head [i' | i' <- [1..length constituents], head (constituents `rotatedBy` (i' - 1)) == lowermost]
              else 1
invert _ _ = error "Control.Template: Unexpected analysis"

-- | Generates an analysis given a root, the best-fit template, and the original music.
realize :: (Root, Template)
        -> Music -- ^ Access to the original musical segment is required to generate the correct accidental.
        -> Analysis
realize (r, t) m
    | MinorSeventh    <- t = Analysis (enh, Min) [Add 7]
    | MajorSeventh    <- t = Analysis (enh, Maj) [Add 7]
    | FullDiminished  <- t = Analysis (enh, Min) [Dim, Adj 7 flat, Add 7]
    | HalfDiminished  <- t = Analysis (enh, Min) [Dim, Add 7]
    | DominantSeventh <- t = Analysis (enh, Maj) [Adj 7 flat, Add 7]
    | AugmentedTriad  <- t = Analysis (enh, Maj) [Aug]
    | DiminishedTriad <- t = Analysis (enh, Min) [Dim]
    | MajorTriad      <- t = Analysis (enh, Maj) []
    | MinorTriad      <- t = Analysis (enh, Min) []
    | Fifth           <- t = Analysis (enh, Maj) [Sub 3]      -- Defaults to major.
    | Tritone         <- t = Analysis (enh, Min) [Sub 3, Dim] -- Defaults to minor.
    | MinorThird      <- t = Analysis (enh, Min) [Sub 5]
    | MajorThird      <- t = Analysis (enh, Maj) [Sub 5]
    | Atom            <- t = Analysis (enh, Maj) []           -- Defaults to major.
    | otherwise            = error "Control.Template: Empty template"
    where -- Collects the most prevalent (i.e. repeating) notes over the original music.
          enhs = [enh' | (enh', w) <- sortBy (comparing snd <> comparing fst) $ occurrences m
                       , w == (fst . maximum . map swap) (occurrences m)]
          -- Then, retrieves the appropriate accidental for the root.
          enh = fromMaybe (head . scale $ (head enhs, Maj)) (find (\enh' -> pitchClassOf enh' == r) enhs)

weight :: Music -> Vector
weight = fill . map (first pitchClassOf) . occurrences
    where fill w = [(s, fromMaybe (0 :: Int) (lookup s w))
                   | s <- [0..11]]

score :: Vector -> Template -> (Score, Root)
score w t =
    let ts = take 12 (iterate (map succ) t)
        ss = [(score' w t',
               toMod (fromJust (t' `elemIndex` ts)))
             | t' <- ts]
    in maximum ss
    where score' :: Vector -> Template -> Score
          score' []          _  = 0
          score' ((s, c):w') t' = if s `elem` t'
                                  then if c /= 0
                                       -- Positive evidence:
                                       then c + score' w' t'
                                       -- Misses:
                                       else -1 + score' w' t'
                                  -- Negative evidence:
                                  else -c + score' w' t'

pattern Atom :: Template
pattern Atom = [0]

pattern MajorThird :: Template
pattern MajorThird = [0, 4]

pattern MinorThird :: Template
pattern MinorThird = [0, 3]

pattern Tritone :: Template
pattern Tritone = [0, 6]

pattern Fifth :: Template
pattern Fifth = [0, 7]

pattern MajorTriad :: Template
pattern MajorTriad = [0, 4, 7]

pattern MinorTriad :: Template
pattern MinorTriad = [0, 3, 7]

pattern DiminishedTriad :: Template
pattern DiminishedTriad = [0, 3, 6]

pattern AugmentedTriad :: Template
pattern AugmentedTriad = [0, 4, 8]

pattern DominantSeventh :: Template
pattern DominantSeventh = [0, 4, 7, 10]

pattern HalfDiminished :: Template
pattern HalfDiminished = [0, 3, 6, 10]

pattern FullDiminished :: Template
pattern FullDiminished = [0, 3, 6, 9]

pattern MajorSeventh :: Template
pattern MajorSeventh = [0, 4, 7, 11]

pattern MinorSeventh :: Template
pattern MinorSeventh = [0, 3, 7, 10]