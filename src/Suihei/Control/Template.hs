{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Template (
    label
) where

import Suihei.Control.Prelude (
    Semitonal (pitchClassOf),
    occurrences, rotatedBy, scale, notes)
import Suihei.Control.Reification (reify)
import Suihei.Data.Chromatic (Semitone, flat)
import Suihei.Data.Chordal (
    Mode (..),
    Analysis, pattern Analysis,
    Variation (..),
    pattern Dim, pattern Aug, base, TAnalysis)
import Suihei.Data.Composition (
    Total (duration),
    pattern Unit,
    Music)

import Suihei.Text.Prettyprinter ()

import Data.Bifunctor (first)
import Data.List (find, sortBy, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Data.Ord (comparing, Down (Down))

type Template = [Semitone]

type Score = Int

type Weight = Int
type Vector = [(Semitone, Weight)]

type Root = Semitone

label :: Music -> (Score, TAnalysis)
label m = let w = weight m in
          if null (occurrences m)
          -- TODO: In the future, music only consisting of rests should error, reasonably.
          then (minBound, (Analysis base [], duration m)) -- If the music is a rest, return a value that will always be merged.
          else let ((sc, r), t) = maximum $ map ((,) =<< score w) preference
                   inv          = invert (realize (r, t) m) m
               in (sc, (inv, duration m))
    where preference = [ MinorSeventh, MajorSeventh, FullDiminished, HalfDiminished, DominantSeventh
                       , AugmentedTriad, DiminishedTriad, MinorTriad, MajorTriad
                       , Fifth, Tritone
                       , MinorThird, MajorThird
                       , Atom ]

invert :: Analysis -> Music -> Analysis
invert ana@(Analysis key vs) m = if i == 1
                                 then ana
                                 else Analysis key (Inv i : vs)
    where lowermost    = snd . minimum . map swap $ notes m
          constituents = map fst . notes . reify $ Unit (ana, 0)
          i = if lowermost `elem` constituents
              then head [i' | i' <- [1..length constituents]
                            , head (constituents `rotatedBy` (i' - 1)) == lowermost]
              else 1
invert _ _ = error "Control.Template: Unexpected shift"

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
          enhs = map fst (sortBy (comparing snd) $ occurrences m)
          -- Then, retrieves the appropriate accidental for the root.
          -- If the computed root is omitted (could occur due to ambuigity), then default to the lowest note.
          enh = fromMaybe (head . scale $ (head enhs, Maj)) 
                          (find (\e -> pitchClassOf e == r) enhs)

weight :: Music -> Vector
weight = fill . map (first pitchClassOf) . occurrences
    where fill w = [(s, fromMaybe (0 :: Weight) (lookup s w))
                   | s <- [0..11]]

score :: Vector -> Template -> (Score, Root)
score w t =
    let ts :: [Template]
        ts = take 12 (iterate (map succ) t)
        ss :: [(Score, Root)]
        ss = zip (score' w <$> ts) [0..11]
    in maximumBy (\s1 s2 -> comparing fst s1 s2 
                         <> compare (Down $ snd s1) (Down $ snd s2)) ss
    where score' :: Vector -> Template -> Score
          score' []          _  = 0
          score' ((s, w'):w'') t' = if s `elem` t'
                                    then 
                                        if w' /= 0
                                        -- Positive evidence:
                                        then w' + score' w'' t'
                                        -- Misses:
                                        else -1 + score' w'' t'
                                    -- Negative evidence:
                                    else -w' + score' w'' t'

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