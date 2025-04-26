{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Calla.Text.Prettyprinter (Pretty) where

import Data.Char (toLower)
import Data.Maybe (fromJust)
import Prettyprinter

import Calla.Control.Prelude (unchroma)
import Calla.Data.Chordal (
    Key, Mode (..), 
    Analysis, pattern Analysis, 
    Variation (..))
import Calla.Data.Chromatic (Diatone(..), Accidental (..), diatones)
import Calla.Data.Notal (Note)

instance Pretty Accidental where
    pretty Flat  = pretty "b"
    pretty Nat   = emptyDoc
    pretty Sharp = pretty "#" 

instance Pretty Key where
    pretty k 
        | (a, d, m) <- unchroma k 
        = pretty a <> mode m (fromJust (lookup d numeral_))
        where mode Maj = id
              mode Min = pretty . map toLower . show

instance Pretty Analysis where
    pretty (Analysis k vs) = pretty k <> mconcat (map pretty vs)

instance Pretty Variation where
    pretty Dim = pretty "o"
    pretty Aug = pretty "+"
    pretty (Snd k) = pretty "/" <> pretty k
    pretty (Add i) = brackets (pretty "a"<> pretty i)
    pretty (Sub i) = brackets (pretty "no" <> pretty i)
    pretty (Inv i) = brackets (pretty "i" <> pretty i)

instance Pretty Note where
    pretty n = fromJust (lookup d note_) <> pretty a <> pretty o
        where (a, d, o) = unchroma n

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