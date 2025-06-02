{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Suihei.Control.Compiler (Compile (..)) where

import Suihei.Control.Prelude (relative)
import Suihei.Control.Reification (reify)
import Suihei.Data.Abstract (File, Value (..))
import Suihei.Data.Chromatic (Enharmonic, Accidental)
import Suihei.Data.Modular (fromMod)
import Suihei.Data.Chordal (
    Key, Mode (..),
    Analysis, pattern Analysis, Variation (..),
    pattern Dim, pattern Aug, base)
import Suihei.Data.Notal (Octave, Note, pattern Rest)
import Suihei.Data.Composition (
    fore, duration,
    Progression, Music,
    pattern Unit, pattern (:>), pattern (:^))
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Prettyprinter ()

import Prettyprinter
import Prettyprinter.Render.Text (hPutDoc)

import Data.Bits ((.&.))
import Data.Ratio (numerator, denominator, (%))
import Data.List (elemIndex, (\\), delete)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import System.IO (withFile, IOMode (..))
import Data.List.Extra (notNull)

class Compile a where
    compile  :: a -> Key -> Doc ann
    cout     :: a -> Key -> File -> IO ()

    default cout     :: a -> Key -> File -> IO ()
    cout a key file'
        | Nothing   <- file' = withFile "a.ly"          WriteMode (`hPutDoc` doc)
        | Just file <- file' = withFile (file ++ ".ly") WriteMode (`hPutDoc` doc)
        where doc = compile a key
    {-# MINIMAL compile #-}

command_ :: String -> Doc ann
command_ s = backslash <> pretty s

fixed_ :: Doc ann -- ^ Content outside of the \score block
       -> Doc ann -- ^ Content inside of the \score block
       -> Doc ann
fixed_ d d' = vsep [ command_ "version"  <+> dquotes (pretty "2.24.4")
                   , d
                   , command_ "score" <+> lbrace
                   , indent 4 d'
                   , indent 4 (command_ "layout") <+> lbrace <+> rbrace
                   , indent 4 (command_ "midi") <+> lbrace <+> rbrace
                   , rbrace ]

music_ :: Key -> Doc ann -> Doc ann
music_ key d = vsep [ command_ "transpose" <+> pretty "c" <+> format key <+> lbrace
                    , indent 4 (command_ "fixed c'" <+> lbrace)
                    , indent 8 (command_ "tempo" <+> pretty "4" <+> equals <+> pretty "120")
                    , indent 8 (command_ "key" <+> pretty "c" <+> command_ "major")
                    , indent 8 d
                    , indent 4 rbrace
                    , rbrace ]

instance Compile Value where
    compile (Music m) key       = fixed_ emptyDoc $ compile m key
    compile (Progression p) key = fixed_ emptyDoc $ compile (reify p) key

instance Compile Music where
    compile m key = music_ key (compile' m)
        where compile' :: Music -> Doc ann
              compile' (Unit (note, dur))
                  | Rest            <- note = format ("r", dur)
                  | Just (enh, oct) <- note = format (show . format $ (enh, oct), dur)
              compile' (m1 :> m2) = compile' m1 <+> compile' m2
              compile' (m1 :^ m2)
                  | Just ms <- chord_ (m1 :^ m2) = 
                      format (show $ angles_ (concatWith (<+>) (map (format . note_) ms)), 
                              duration (head ms))
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

type Annotated = (Progression, Music)

annotated_ :: Doc ann
annotated_ = vsep [ llangle_
                  , indent 4 $ command_ "new" <+> pretty "Staff = \"comp\"" <+> command_ "comp"
                  , indent 4 $ command_ "new" <+> pretty "Dynamics = \"rn\"" <+> command_ "rn"
                  , rrangle_ ]

instance Compile Annotated where
    compile (p, m) key = fixed_ (vsep [d1, d2]) annotated_
        where d1 = line 
                <> pretty "comp =" <+> compile m base
              d2 = line 
                <> pretty "rn =" 
                <+> vsep [lbrace, indent 4 $ compile p key, rbrace]
                <> line

instance Compile Progression where
    compile (Unit (ana, dur)) _ = format ("s", dur) 
                               <> pretty "-"
                               <> command_ "markup"
                              <+> command_ "upright"
                              <+> dquotes (format ana)
    compile (p1 :> p2) key = compile p1 key <+> compile p2 key
    compile _ _ = undefined

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

instance Format_ Analysis where
    format (Analysis key vs) = pretty key <> format vs
    format _ = undefined

instance Format_ [Variation] where
    format vs
        | null vs = emptyDoc
        | Dim `elem` vs = pretty "o"
                       <> format (Dim `delete` vs) 
        | Aug `elem` vs = pretty "+"
                       <> format (Aug `delete` vs)
        | Adj 7 (-1) `elem` vs 
       && Add 7      `elem` vs = pretty "7" 
                              <> format (vs \\ [Adj 7 (-1), Add 7])
        | is <- [i | Inv i <- vs]
        , notNull is = parens (pretty "i" 
                    <> pretty (sum is - 1))
                    <> format (vs \\ map Inv is)
        | as <- [a | Add a <- vs]
        , notNull as = parens ((hsep . punctuate comma) (map ((pretty "add" <>) . pretty) as))
                    <> format (vs \\ map Add as)
        | ss <- [s | Sub s <- vs]
        , notNull ss = parens ((hsep . punctuate comma) (map ((pretty "no" <>) . pretty) ss))
                    <> format (vs \\ map Sub ss)
        | otherwise = undefined

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

-- Here, the pretty-printed note must be passsed in due to the tuplet syntax.
instance Format_ (String, Duration) where
    format (doc, dur)
        -- If the duration is greater than a whole note, create a tie
        | dur > 1 = pretty doc <> pretty "1~" <+> format ("", dur - 1)
        -- If the duration is one of 1/1, 1/2, 1/4, ..., 1/1024
        | numerator dur == 1
       && dur `elem` durs = pretty doc <> pretty (denominator dur)
        -- If augmentation dots apply
        | Just (dur', dots') <- dots = pretty doc
                                    <> pretty (denominator dur')
                                    <> hcat (replicate dots' dot)
        | otherwise = command_ "tuplet" 
                  <+> pretty (fst $ tuplet dur) 
                  <+> braces (pretty doc <> format ("", snd $ tuplet dur))
        where -- Retrieves only the powers of two from 1-1024.
              series :: [Int]
              series = filter (\n -> n .&. (n - 1) == 0) [1..1024]
              durs :: [Duration]
              durs = map (1 %) series
              -- Determines 1) whether augmentation dots apply, 2) the number of augmentation dots.
              dots :: Maybe (Duration, Int)
              dots = let near   = maximum (filter (<= dur) durs)
                         dotted = scanl1 (+) [near * n | n <- durs] in
                         if dur `elem` dotted
                         then Just (near, fromJust $ dur `elemIndex` dotted)
                         else Nothing
              -- Determines the appropriate tuplet for an arbitrary duration, also returning the closest duration to said duration.
              tuplet :: Duration -> (Duration, Duration)
              tuplet dur' = let near = maximum (filter (<= dur') durs) in
                            ((denominator dur' % numerator dur') * near, near)

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