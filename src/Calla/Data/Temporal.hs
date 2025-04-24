module Calla.Data.Temporal (
    Duration,
    Temporal, Copuntal, Contrapuntal
) where

import Data.Ratio (Ratio)

type Duration = Ratio Int

type Temporal a     = (a, Duration)
type Copuntal a     = [Temporal a]
type Contrapuntal a = [Copuntal [a]]