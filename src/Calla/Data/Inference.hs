{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.Inference (Inference) where

import Calla.Data.Chordal (Analysis, pattern Analysis)
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive (Context)
import Calla (Chromatic, Mode (..))

type Weight = Int

-- | A set of progressions is a directed acyclic graph with analyses for nodes.
type Inference = Gr Analysis Weight