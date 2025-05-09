{-# LANGUAGE PatternSynonyms #-}

module Calla.Data.HGraph (
    Probability, HGraph,
    dijkstra
) where

import Calla.Data.Chordal (Analysis, pattern Analysis)

import Data.Graph.Inductive.Graph (Graph, Node, Path)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.Tree (Gr)

type Probability = Float

-- | A set of progressions (i.e. harmonies) is a directed acyclic graph with analyses for nodes.
type HGraph = Gr Analysis Probability

-- | A synonym for @sp@ in @fgl@'s @Data.Graph.Inductive.Query.SP@ module.
dijkstra :: (Graph gr, Real b)
          => Node -- ^ Origin
          -> Node -- ^ Destination
          -> gr a b
          -> Maybe Path
dijkstra = sp