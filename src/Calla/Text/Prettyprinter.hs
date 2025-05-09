{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Calla.Text.Prettyprinter (Pretty) where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Prettyprinter

import Calla.Data.Chordal (
    Key, Mode (..),
    Analysis  (..), pattern Analysis,
    Variation (..))
import Calla.Data.Chromatic (Diatone, diatones, Accidental)
import Calla.Data.Notal (Note)
import Calla.Data.Sequence (
    Duration,
    pattern Unit, pattern (:>), pattern (:^), 
    Progression, Music)

import Data.Ratio (numerator, denominator)

accidental :: Accidental -> Doc ann
accidental a
    | a < 0     = pretty "b" <> accidental (a + 1)
    | a > 0     = pretty "#" <> accidental (a - 1)
    | otherwise = emptyDoc

instance {-# OVERLAPS #-} Pretty Key where
    pretty (a, d, m) = accidental a <> mode m (fromJust (lookup d numeral_))
        where mode Maj = id
              mode Min = pretty . map toLower . show

instance Pretty Analysis where
    pretty (Analysis k vs) = pretty k <> mconcat (map pretty vs)
    pretty (Shift k a)     = pretty k <+> colon <+> pretty a

instance Pretty Variation where
    pretty Dim = pretty "o"
    pretty Aug = pretty "+"
    pretty (Snd k) = pretty "/" <> pretty k
    pretty (Add i) = brackets (pretty "a"<> pretty i)
    pretty (Sub i) = brackets (pretty "no" <> pretty i)
    pretty (Inv i) = brackets (pretty "i" <> pretty i)

instance {-# OVERLAPS #-} Pretty Note where
    pretty (a, d, o) = fromJust (lookup d note_) <> accidental a <> pretty o

data Numeral_ = I | II | III | IV
              | V | VI | VII deriving (Enum, Show)

numeral_ :: [(Diatone, Doc ann)]
numeral_ = zip_ (enumFromTo I VII)

data Note_ = C | D | E | F
           | G | A | B deriving (Enum, Show)

note_ :: [(Diatone, Doc ann)]
note_ = zip_ (enumFromTo C B)

zip_ :: Show a => [a] -> [(Diatone, Doc ann)]
zip_ xs = zip diatones (map (pretty . show) xs)

instance Pretty Duration where
    pretty d = (pretty . numerator) d <> pretty "/" <> (pretty . denominator) d

instance Pretty Progression where
    pretty (Unit a d) = pretty a <+> pretty d
    pretty (p1 :> p2) = pretty p1 <+> pretty ">" <+> pretty p2

instance Pretty Music where
    pretty (Unit n d) = pretty n <+> pretty d
    pretty (m1 :> m2) = pretty m1 <+> pretty ">" <+> pretty m2
    pretty (m1 :^ m2) = pretty m1 <+> pretty "^" <+> pretty m2