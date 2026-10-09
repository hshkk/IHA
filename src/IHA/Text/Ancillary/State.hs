module IHA.Text.Ancillary.State (
    Propagation,
    getVariations,  modifyVariations,
    getOctaviation, modifyOctaviation,
    getDuration,    modifyDuration,
    reset,
    empty
) where

import IHA.Data.Chordal (Variation)
import IHA.Data.Notal (Octave)
import IHA.Data.Temporal (Duration)
import IHA.Text.Ancillary.Monad (Parser, Propagation)

import Text.Parsec.Prim (getState, modifyState)

getVariations :: Parser (Maybe [Variation])
getVariations = getState >>= \(vars, _, _) -> return vars

getOctaviation :: Parser (Maybe Octave)
getOctaviation = getState >>= \(_, oct, _) -> return oct

getDuration :: Parser (Maybe Duration)
getDuration = getState >>= \(_, _, dur) -> return dur

reset :: Parser ()
reset = do
    modifyVariations Nothing
    modifyOctaviation Nothing
    modifyDuration Nothing

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