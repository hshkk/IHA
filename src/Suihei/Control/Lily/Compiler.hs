{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Suihei.Control.Lily.Compiler where

import Suihei.Control.Compiler (Compile (..))
import Suihei.Data.Chromatic (Enharmonic, Accidental)
import Suihei.Data.Modular (fromMod)
import Suihei.Data.Notal (Octave)
import Suihei.Data.Composition (
    fore, duration,
    Music,
    pattern Unit, pattern (:>), pattern (:^))
import Suihei.Data.Temporal (Duration)

import Prettyprinter

import Data.Bits ((.&.))
import Data.Ratio (numerator, denominator, (%))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

fixed :: Doc ann -> Doc ann
fixed s = vsep [ backslash <> pretty "version"  <+> dquotes (pretty "2.24.4")
               , emptyDoc
               , backslash <> pretty "score" <+> lbrace
               , indent 4 (backslash <> pretty "fixed c'" <+> lbrace)
               , indent 8 s
               , indent 4 rbrace
               , rbrace ]

llangle :: Doc ann
llangle = langle <> langle

rrangle :: Doc ann
rrangle = rangle <> rangle

bbackslash :: Doc ann
bbackslash = backslash <> backslash

instance Compile Music where
    compile m = fixed (compile' m)
        where compile' :: Music -> Doc ann
              compile' (Unit (note, dur))
                  | Nothing         <- note = pretty "r" 
                                           <> format dur
                  | Just (enh, oct) <- note = format enh 
                                           <> format oct
                                           <> format dur
              compile' (m1 :> m2) = compile' m1 <+> compile' m2
              compile' (m1 :^ m2)
                  | Just ms <- chord (m1 :^ m2) = llangle
                                              <+> concatWith (<+>) (map compile' ms)
                                              <+> bbackslash
                                              <+> braces emptyDoc
                                              <+> rrangle
                  | otherwise = let m1' = if duration (fore m1) < duration (fore m2)
                                          then m1
                                          else m2
                                    m2' = if duration (fore m1') == duration (fore m1)
                                          then m2
                                          else m1 in 
                                llangle
                            <+> braces (compile' m2')
                            <+> bbackslash
                            <+> braces (compile' m1')
                            <+> rrangle
              compile' _ = undefined
    suffix _   = ".ly"

chord :: Music -> Maybe [Music]
chord unit@(Unit _) = Just [unit]
chord (m1 :^ m2)
    | Just ms@(Unit m : ms') <- (++) <$> chord m1 <*> chord m2
    , all (\(Unit m') -> snd m == snd m') ms = Just ms
    | otherwise = Nothing
chord _ = Nothing

class Format_ a where
    format :: a -> Doc ann

instance Format_ Enharmonic where
    format (dia, acc)
        | acc < 0   = if acc < -2
                      then format (dia - 1, acc + 2)
                      else format (dia, 0 :: Accidental)
                        <> hcat (replicate (abs acc) (pretty "es"))
        | acc > 0   = if acc > 2
                      then format (dia + 1, acc - 2)
                      else format (dia, 0 :: Accidental)
                        <> hcat (replicate acc (pretty "is"))
        | otherwise = notes !! fromMod dia
        where notes = map pretty "cdefgab"

instance Format_ Octave where
    format oct'
        | oct' < 4  = comma  <> format (oct' + 1)
        | oct' > 4  = squote <> format (oct' - 1)
        | otherwise = emptyDoc

instance Format_ Duration where
    format dur
        | numerator dur == 1
       && dur `elem` durs = pretty (denominator dur)
        -- Augmentation dots
        | Just (dur', dots') <- dots = pretty (denominator dur')
                                    <> hcat (replicate dots' dot)
        | otherwise = error "Control.Lily.Compiler: Invalid duration"
        where series :: [Int]
              series = filter (\n -> n .&. (n - 1) == 0) [1..1024]
              durs :: [Duration]
              durs = map (1 %) series
              dots :: Maybe (Duration, Int)
              dots = let near   = maximum (filter (<= dur) durs)
                         dotted = scanl1 (+) [near * n | n <- durs] in
                         if dur `elem` dotted
                         then Just (near, fromJust $ dur `elemIndex` dotted)
                         else Nothing