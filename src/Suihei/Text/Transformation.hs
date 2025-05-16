module Suihei.Text.Transformation (
    returnM,
    getVariations, getOctaviation, getDuration,
    modifyVariations, modifyOctaviation, modifyDuration,
    empty,
    Suihei, SuiheiOperatorTable
) where

import Suihei.Data.Chordal (Variation)
import Suihei.Data.Notal (Octave)
import Suihei.Data.Temporal (Duration)

import Text.Parsec (getState, modifyState)
import Text.Parsec.Expr (OperatorTable)
import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

returnM :: Monad m => a -> m (Maybe a)
returnM a = return (Just a)

type Propagation_ = (Maybe [Variation], Maybe Octave, Maybe Duration)

getVariations :: Suihei (Maybe [Variation])
getVariations = do
    (vars, _, _) <- getState
    return vars

getOctaviation :: Suihei (Maybe Octave)
getOctaviation = do
    (_, oct, _) <- getState
    return oct

getDuration :: Suihei (Maybe Duration)
getDuration = do
    (_, _, dur) <- getState
    return dur

-- | Performs a direct substitution on the active propagated variations.
modifyVariations :: Maybe [Variation] -> Suihei ()
modifyVariations vars = modifyState (\(_, oct, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated octaviation.
modifyOctaviation :: Maybe Octave -> Suihei ()
modifyOctaviation oct = modifyState (\(vars, _, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated duration.
modifyDuration :: Maybe Duration -> Suihei ()
modifyDuration dur = modifyState (\(vars, oct, _) -> (vars, oct, dur))

-- | The initial parser state with no active propagations.
empty :: Propagation_
empty = (Nothing, Nothing, Nothing)

type Suihei a              = ParsecT       String Propagation_ Identity a
type SuiheiOperatorTable a = OperatorTable String Propagation_ Identity a