module Suihei.Text.Ancillary.Monad (
    Propagation,
    SuiheiParser,
    SuiheiOperatorTable
) where

import Suihei.Data.Chordal (Variation)
import Suihei.Data.Notal (Octave)
import Suihei.Data.Temporal (Duration)

import Text.Parsec.Expr (OperatorTable)
import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

type Propagation = (Maybe [Variation], Maybe Octave, Maybe Duration)

type SuiheiParser a        = ParsecT       String Propagation Identity a
type SuiheiOperatorTable a = OperatorTable String Propagation Identity a