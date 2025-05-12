module Suihei.Data.Temporal (
    Duration,
    Temporal
) where

import Data.Ratio (Ratio)

type Duration = Ratio Int

type Temporal a = (a, Duration)