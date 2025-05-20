{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Suihei.Control.Lily.Compiler () where

import Suihei.Control.Compiler (Compile (..))
import Suihei.Data.Chromatic (Enharmonic, Accidental)
import Suihei.Data.Modular (fromMod)
import Suihei.Data.Chordal (Key, Mode (..))
import Suihei.Data.Notal (Octave, Note, pattern Rest)
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
import Data.Tuple (swap)
import Suihei.Control.Prelude (relative)
import Suihei.Data.Abstract (Value (..))

command_ :: String -> Doc ann
command_ s = backslash <> pretty s

fixed_ :: Key -> Doc ann -> Doc ann
fixed_ key d = vsep [ command_ "version"  <+> dquotes (pretty "2.24.4")
                    , emptyDoc
                    , command_ "score" <+> lbrace
                    , indent 4 (command_ "transpose" <+> pretty "c" <+> format key <+> lbrace)
                    , indent 8 (command_ "fixed c'" <+> lbrace)
                    , indent 12 (command_ "tempo" <+> pretty "4" <+> equals <+> pretty "120")
                    , indent 12 (command_ "key" <+> pretty "c" <+> command_ "major")
                    , indent 12 d
                    , indent 8 rbrace
                    , indent 4 rbrace
                    , indent 4 (command_ "layout") <+> lbrace <+> rbrace
                    , indent 4 (command_ "midi") <+> lbrace <+> rbrace
                    , rbrace ]

instance Compile Value where
    compile (Music m) key       = compile m key
    compile (Progression p) key = undefined

instance Compile Music where
    compile m key = fixed_ key (compile' m)
        where compile' :: Music -> Doc ann
              compile' (Unit (note, dur))
                  | Rest            <- note = 
                      pretty "r" 
                   <> format dur
                  | Just (enh, oct) <- note = 
                      format (enh, oct) 
                   <> format dur
              compile' (m1 :> m2) = compile' m1 <+> compile' m2
              compile' (m1 :^ m2)
                  | Just ms <- chord_ (m1 :^ m2) = 
                      angles_ (concatWith (<+>) (map (format . note_) ms))
                   <> format (duration (head ms))
                  | otherwise =
                      let m1' = if octaviation_ (fore m1) < octaviation_ (fore m2)
                                then m2
                                else m1
                          m2' = if octaviation_ (fore m1) < octaviation_ (fore m2)
                                then m1
                                else m2 in 
                      aangles_ (braces (command_ "voiceOne" <+> compile' m1')
                            <+> bbackslash_
                            <+> braces (command_ "voiceTwo" <+> compile' m2'))
                            <>  line
              compile' _ = undefined

-- | Rewrites chords as a list of units.
-- Chords are defined here as parallel compositions with no serial compositions, with all constituents having the same duration.
chord_ :: Music -> Maybe [Music]
chord_ unit@(Unit _) = Just [unit]
chord_ (m1 :^ m2)
    | Just ms@(Unit m : ms') <- (++) <$> chord_ m1 <*> chord_ m2
    , all (\(Unit m') -> snd m == snd m') ms' = Just ms
    | otherwise = Nothing
chord_ _ = Nothing

-- | Extracts the constituent note from a (unit) musical object.
-- @note_@ is only used when there is a guarantee that the argument will not fail the initial pattern match.
note_ :: Music -> Note
note_ (Unit (Just note, _)) = note
note_ _ = undefined

octaviation_ :: Music -> Maybe (Octave, Enharmonic)
octaviation_ (Unit (note, _)) = swap <$> note
octaviation_ (m1 :^ m2) = min (octaviation_ m1)(octaviation_ m2)
octaviation_ (m1 :> m2) = min (octaviation_ m1)(octaviation_ m2)

class Format_ a where
    format :: a -> Doc ann

instance Format_ Key where
    format (enh, Maj) = format enh
    format key        = format (maj, Maj)
        where maj = fst (relative key)

instance Format_ Note where
    format (enh, oct) = format enh <> format oct

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

llangle_ :: Doc ann
llangle_ = langle <> langle

rrangle_ :: Doc ann
rrangle_ = rangle <> rangle

angles_ :: Doc ann -> Doc ann
angles_ d = langle <+> d <+> rangle

aangles_ :: Doc ann -> Doc ann
aangles_ d = llangle_ <+> d <+> rrangle_

bbackslash_ :: Doc ann
bbackslash_ = backslash <> backslash