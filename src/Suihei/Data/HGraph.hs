module Suihei.Data.HGraph (
    HGraph,
    dijkstra
) where

import Suihei.Data.Chordal (Analysis)
import Suihei.Data.Probability (Probability)

import Data.Graph.Inductive.Graph (Graph, Node, Path)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Graph.Inductive.Tree (Gr)

-- | A set of progressions (i.e. harmonies) is a directed acyclic graph with analyses for nodes.
type HGraph = Gr Analysis Probability

-- | A synonym for @sp@ in @fgl@'s @Data.Graph.Inductive.Query.SP@ module.
dijkstra :: (Graph gr, Real b)
          => Node -- ^ Origin
          -> Node -- ^ Destination
          -> gr a b
          -> Maybe Path
dijkstra = sp