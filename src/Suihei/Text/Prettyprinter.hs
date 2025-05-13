{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Suihei.Text.Prettyprinter (Pretty) where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Prettyprinter

import Suihei.Data.Chromatic (Diatone, diatones, Accidental)
import Suihei.Data.Chordal (
    Key, Mode (..),
    Analysis  (..), pattern Analysis,
    Variation (..), TAnalysis)
import Suihei.Data.Notal (Note, pattern Rest, TNote)
import Suihei.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Progression, Music)
import Suihei.Data.Temporal (Duration, Temporal)

import Data.Ratio (numerator, denominator)

accidental :: Accidental -> Doc ann
accidental a
    | a < 0     = pretty "b" <> accidental (a + 1)
    | a > 0     = pretty "#" <> accidental (a - 1)
    | otherwise = emptyDoc

instance Pretty Mode where
    pretty Maj = pretty "Maj"
    pretty Min = pretty "Min"

instance {-# OVERLAPS #-} Pretty Key where
    pretty ((d, m), a) = accidental a <> mode m (fromJust (lookup d numeral_))
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
    pretty ((d, o), a) = fromJust (lookup d note_) <> accidental a <> pretty o

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

instance {-# OVERLAPS #-} Pretty a => Pretty (Temporal a) where
    pretty (x, d) = pretty x <+> pretty d

instance Pretty Progression where
    pretty (Unit a) = pretty a
    pretty (p1 :> p2) = pretty p1 <+> pretty ">" <+> pretty p2

instance Pretty Music where
    pretty (Unit n)
        | (Rest, d) <- n = pretty "*" <+> pretty d
        | otherwise      = pretty n
    pretty (m1 :> m2) = pretty m1 <+> pretty ">" <+> pretty m2
    pretty (m1 :^ m2)
        | (_ :> _) <- m2 = pretty m1 <+> pretty "^" <+> (parens . pretty) m2
        | otherwise      = pretty m1 <+> pretty "^" <+> pretty m2

-- Show instance generation could probably be done automatically with Template Haskell, but it isn't too necessary here.

instance Show Mode where
    show = show . pretty

instance {-# OVERLAPS #-} Show Key where
    show = show . pretty

instance Show Analysis where
    show = show . pretty

instance Show Variation where
    show = show . pretty

instance {-# OVERLAPS #-} Show Note where
    show = show . pretty

instance {-# OVERLAPS #-} Show Duration where
    show = show . pretty

instance {-# OVERLAPS #-} Show TAnalysis where
    show = show . pretty

instance {-# OVERLAPS #-} Show TNote where
    show = show . pretty

instance Show Progression where
    show = show . pretty

instance Show Music where
    show = show . pretty