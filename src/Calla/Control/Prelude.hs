module Calla.Control.Prelude (
    unchroma, chroma
) where

import Calla.Data.Chromatic (Chromatic (..), Diatonic (..), Diatone, Semitone)

unchroma :: Chromatic a -> (Semitone, Diatone, a)
unchroma (C s (D d x)) = (s, d, x)

chroma :: (Semitone, Diatone, a) -> Chromatic a
chroma (s, d, x) = C s (D d x)