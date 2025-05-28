{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Suihei.Text.Prettyprinter (Pretty) where

import Suihei.Data.Chromatic (Diatone, diatones, Accidental)
import Suihei.Data.Chordal (
    Key, Mode (..),
    Analysis  (..), pattern Analysis,
    Variation (..),
    pattern Dim, pattern Aug,
    TAnalysis, IAnalysis (Ia))
import Suihei.Data.Notal (Note, pattern Rest, TNote, INote (..))
import Suihei.Data.Composition (
    pattern Unit, pattern (:>), pattern (:^),
    Progression, Music, Composition)
import Suihei.Data.Temporal (Duration, Temporal)

import Prettyprinter

import Data.Char (toLower)
import Data.Maybe (fromJust)
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
    pretty ((d, a), m) = accidental a <> mode m (fromJust (lookup d numeral_))
        where mode Maj = id
              mode Min = pretty . map toLower . show

instance Pretty Analysis where
    pretty (Analysis k vs) = pretty k <> delimit (map pretty vs)
        where delimit docs
                  | null docs = emptyDoc
                  | otherwise = pretty "|" <> (hsep . punctuate comma) docs <> pretty "|"
    pretty (Shift k a)     = pretty k <+> colon <+> pretty a

instance Pretty IAnalysis where
    pretty (Ia (Analysis k vs, dur)) = format k <> delimit (map pretty vs) <+> pretty dur
        where format ((d, a), m) = pretty (names !! fromEnum d) 
                                <> accidental a 
                                <> (if m == Maj then emptyDoc else pretty "m") 
              names = "CDEFGAB"
              delimit docs
                  | null docs = emptyDoc
                  | otherwise = pretty "|" <> (hsep . punctuate comma) docs <> pretty "|"
    pretty (Ia (Shift k a, dur)) = pretty k <+> colon <+> pretty (Ia (a, dur))

instance Pretty Variation where
    pretty Dim         = pretty "o"
    pretty Aug         = pretty "+"
    pretty (Snd k)     = pretty "/" <> pretty k
    pretty (Inv i)     = pretty "i" <> pretty i
    pretty (Adj i acc) = pretty "adj" <> accidental acc <> pretty i
    pretty (Add i)     = pretty "a"<> pretty i
    pretty (Sub i)     = pretty "no" <> pretty i

instance {-# OVERLAPS #-} Pretty Note where
    pretty ((d, a), o) = (pretty . show) (fromEnum d + 1) <> accidental a <> delimit (pretty o)
        where delimit doc = pretty "|" <> doc <> pretty "|"

instance {-# OVERLAPS #-} Pretty INote where
    pretty (In (Nothing, dur))          = pretty "*" <+> pretty dur
    pretty (In (Just ((d, a), o), dur)) = pretty (names !! fromEnum d) <> accidental a <> pretty o <+> pretty dur
        where names = "CDEFGAB"

data Numeral_ = I | II | III | IV
              | V | VI | VII deriving (Enum, Show)

numeral_ :: [(Diatone, Doc ann)]
numeral_ = zip_ (enumFromTo I VII)

zip_ :: Show a => [a] -> [(Diatone, Doc ann)]
zip_ xs = zip diatones (map (pretty . show) xs)

instance Pretty Duration where
    pretty d = (pretty . numerator) d <> pretty "/" <> (pretty . denominator) d

instance {-# OVERLAPS #-} Pretty a => Pretty (Temporal a) where
    pretty (x, d) = pretty x <+> pretty d

instance Pretty Progression where
    pretty (Unit a) = pretty a
    pretty (p1 :> p2) = pretty p1 <+> pretty ">" <+> pretty p2

instance Pretty (Composition IAnalysis) where
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

instance Pretty (Composition INote) where
    pretty (Unit n)
        | (In (Rest, d)) <- n = pretty "*" <+> pretty d
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

instance Show (Composition IAnalysis) where
    show = show . pretty

instance Show Music where
    show = show . pretty

instance Show (Composition INote) where
    show = show . pretty