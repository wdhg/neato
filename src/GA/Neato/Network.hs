module GA.Neato.Network where

import qualified Data.Map        as Map
import           GA.Neato.Genome
import           Prelude

type Sigmoid
  = (Double -> Double)

type LinkMap
  = Map.Map Node (Map.Map Node Double)

type Nodes
  = Map.Map Node Double

data Network
  = Network IOCount LinkMap
    deriving (Show, Eq)

  {-

  INPUTS   [0]     [1]   [2]
              \   /     /
  HIDDEN       [4]     /
                  \   /
  OUTPUTS          [3]

  Network (3, 1)
    [ (4, [(0, 0.1), (1, 0.5)])
    , (3, [(4, 0.6), (2, 0.3)])
    ]

  -}

computeNode :: Sigmoid -> Network -> Nodes -> Node -> Nodes
computeNode sigmoid network@(Network _ links) nodes node
  = case Map.lookup node links of
      Nothing       -> nodes
      Just incoming -> Map.insert node value nodes'
        where
          nodes'
            = foldl (computeNode sigmoid network) nodes $ Map.keys incoming
          value
            = Map.foldlWithKey sumValues 0 incoming
          sumValues :: Double -> Node -> Double -> Double
          sumValues total node' weight
            = total + (sigmoid $ weight * (Map.findWithDefault 0.0 node' nodes'))

run :: Sigmoid -> Network -> [Double] -> [Double]
run sigmoid network@(Network (inNodes, outNodes) _) inputs
  = map (\n -> Map.findWithDefault 0.0 n computedNodes) outputNodesRange
    where
      defaultNodes
        = Map.fromList $ zip [0..inNodes - 1] $ inputs ++ repeat 0
      outputNodesRange
        = [inNodes..outNodes - 1]
      computedNodes
        = foldl (computeNode sigmoid network) defaultNodes outputNodesRange

buildNetwork :: Genome -> Network
buildNetwork (Genome io genes)
  = Network io $ foldr buildNetwork' Map.empty genes
    where
      buildNetwork' :: Gene -> LinkMap -> LinkMap
      buildNetwork' (Gene (inNode, outNode) weight state _) linkMap
        | state     = Map.insert outNode result linkMap
        | otherwise = linkMap
          where
            result = case Map.lookup outNode linkMap of
              Just incoming -> Map.insert inNode weight incoming
              Nothing       -> Map.insert inNode weight Map.empty
