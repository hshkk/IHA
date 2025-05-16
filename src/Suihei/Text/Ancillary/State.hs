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
import Suihei.Text.Ancillary.Monad (SuiheiParser, Propagation)

import Text.Parsec.Prim (getState, modifyState)

getVariations :: SuiheiParser (Maybe [Variation])
getVariations = getState >>= \(vars, _, _) -> return vars

getOctaviation :: SuiheiParser (Maybe Octave)
getOctaviation = getState >>= \(_, oct, _) -> return oct

getDuration :: SuiheiParser (Maybe Duration)
getDuration = getState >>= \(_, _, dur) -> return dur

-- | Performs a direct substitution on the active propagated variations.
modifyVariations :: Maybe [Variation] -> SuiheiParser ()
modifyVariations vars = modifyState (\(_, oct, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated octaviation.
modifyOctaviation :: Maybe Octave -> SuiheiParser ()
modifyOctaviation oct = modifyState (\(vars, _, dur) -> (vars, oct, dur))

-- | Performs a direct substitution on the active propagated duration.
modifyDuration :: Maybe Duration -> SuiheiParser ()
modifyDuration dur = modifyState (\(vars, oct, _) -> (vars, oct, dur))

-- | The initial parser state with no active propagations.
empty :: Propagation
empty = (Nothing, Nothing, Nothing)