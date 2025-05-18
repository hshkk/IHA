module Suihei.Text.Ancillary.State (
    Propagation,
    getVariations,  modifyVariations,
    getOctaviation, modifyOctaviation,
    getDuration,    modifyDuration,
    empty
) where

import Suihei.Data.Chordal (Variation)
import Suihei.Data.Notal (Octave)
import Suihei.Data.Temporal (Duration)
import Suihei.Text.Ancillary.Monad (Parser, Propagation)

import Text.Parsec.Prim (getState, modifyState)

getVariations :: Parser (Maybe [Variation])
getVariations = getState >>= \(vars, _, _) -> return vars

getOctaviation :: Parser (Maybe Octave)
getOctaviation = getState >>= \(_, oct, _) -> return oct

getDuration :: Parser (Maybe Duration)
getDuration = getState >>= \(_, _, dur) -> return dur

-- | Performs a direct substitution on the active propagated variations.
modifyVariations :: Maybe [Variation] -> Parser ()
modifyVariations vars = modifyState (\(_, oct, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated octaviation.
modifyOctaviation :: Maybe Octave -> Parser ()
modifyOctaviation oct = modifyState (\(vars, _, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated duration.
modifyDuration :: Maybe Duration -> Parser ()
modifyDuration dur = modifyState (\(vars, oct, _) -> (vars, oct, dur))

-- | The initial parser state with no active propagations.
empty :: Propagation
empty = (Nothing, Nothing, Nothing)