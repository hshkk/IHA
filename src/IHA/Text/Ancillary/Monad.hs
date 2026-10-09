module IHA.Text.Ancillary.Monad (
    Propagation,
    Parser,
    OpTable
) where

import IHA.Data.Chordal (Variation)
import IHA.Data.Notal (Octave)
import IHA.Data.Temporal (Duration)

import Text.Parsec.Expr (OperatorTable)
import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

type Propagation = (Maybe [Variation], Maybe Octave, Maybe Duration)

type Parser  a = ParsecT       String Propagation Identity a
type OpTable a = OperatorTable String Propagation Identity a