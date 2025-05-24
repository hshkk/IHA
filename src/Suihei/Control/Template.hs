{-# LANGUAGE PatternSynonyms #-}

module Suihei.Control.Template (
    label, 
    pattern MinorSeventh, pattern MajorSeventh, 
    pattern FullDiminished, pattern HalfDiminished, 
    pattern DominantSeventh,
    pattern AugmentedTriad, pattern DiminishedTriad, 
    pattern MinorTriad, pattern MajorTriad,
    pattern Fifth, pattern Tritone, 
    pattern MinorThird, pattern MajorThird, 
    pattern Atom
) where

import Suihei.Control.Prelude (occurrences, Semitonal (pitchClassOf))
import Suihei.Data.Chromatic (Semitone)
import Suihei.Data.Composition (Music, Progression)
import Suihei.Data.Modular (toMod)

import Data.Bifunctor (first)
import Data.List.Extra (elemIndex)
import Data.Maybe (fromMaybe, fromJust)

type Template = [Semitone]

type Score = Int

type Weight = Int
type Vector = [(Semitone, Weight)]

type Root = Semitone

label :: Music -> Progression
label m =
    let highest = maximum (map (\p -> (score (weight m) p, p)) preference)
        likely  = first snd highest in
    undefined
    where preference = [ MinorSeventh, MajorSeventh, FullDiminished, HalfDiminished, DominantSeventh
                       , AugmentedTriad, DiminishedTriad, MinorTriad, MajorTriad
                       , Fifth, Tritone
                       , MinorThird, MajorThird
                       , Atom ]

weight :: Music -> Vector
weight = fill . map (first pitchClassOf) . occurrences
    where fill w = [(s, fromMaybe (0 :: Int) (lookup s w))
                   | s <- [0..11]]

score :: Vector -> Template -> (Score, Root)
score w f =
    let fs = take 12 (iterate (map (+ 1)) f)
        ss = [(score' w f',
               toMod (fromJust (f' `elemIndex` fs)))
             | f' <- fs]
    in maximum ss
    where score' []          _  = 0
          score' ((s, c):w') f' = if s `elem` f'
                                  then if c /= 0
                                       -- Positive evidence:
                                       then c + score' w' f'
                                       -- Misses:
                                       else -1 + score' w' f'
                                  -- Negative evidence:
                                  else -c + score' w' f'

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