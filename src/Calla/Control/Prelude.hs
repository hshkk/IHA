module Calla.Control.Prelude (
    unchroma, chroma
) where

import Calla.Data.Chromatic (Chromatic (..), Diatonic (..), Diatone, Semitone)

unchroma :: Chromatic a -> (Semitone, Diatone, a)
unchroma (C s (D d a)) = (s, d, a)

chroma :: (Semitone, Diatone, a) -> Chromatic a
chroma (s, d, a) = C s (D d a)