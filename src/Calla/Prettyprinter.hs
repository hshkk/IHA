{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Calla.Prettyprinter where

import Calla.Internal.Chromatic

import Data.Char (toLower)
import Data.Ratio (numerator, denominator)
import Prettyprinter

import qualified Data.Set as Set

-- To be implemented!