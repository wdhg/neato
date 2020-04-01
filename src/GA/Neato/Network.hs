module GA.Neato.Network where

import qualified Data.Map        as Map
import           GA.Neato.Genome
import           Prelude

type Sigmoid
  = (Double -> Double)

type LinkMap
  = Map.Map Node (Map.Map Node Double)

type Nodes
  = [Double]

data Network
  = Network IOCount LinkMap
    deriving (Show)

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

setNode :: Node -> Double -> Nodes -> Nodes
setNode index value nodes
  = before ++ (value : after)
    where
      (before, _ : after)
        = splitAt index nodes

computeNode :: Sigmoid -> Network -> Nodes -> Node -> Nodes
computeNode sigmoid network@(Network _ links) nodes node
  = case Map.lookup node links of
      Nothing       -> nodes
      Just incoming -> setNode node value nodes'
        where
          nodes'
            = foldl (computeNode sigmoid network) nodes $ Map.keys incoming
          value
            = Map.foldlWithKey sumValues 0 incoming
          sumValues :: Double -> Node -> Double -> Double
          sumValues total node' weight
            = total + (sigmoid $ weight * (nodes' !! node'))

run :: Sigmoid -> Network -> [Double] -> [Double]
run sigmoid network@(Network (inNodes, outNodes) _) inputs
  = take outNodes $ drop inNodes $ computedNodes
    where
      computedNodes
        = foldl (computeNode sigmoid network) (inputs ++ repeat 0) [inNodes..outNodes - 1]

buildNetwork :: Genome -> Network
buildNetwork (Genome io genes)
  = Network io $ foldr buildNetwork' Map.empty genes
    where
      buildNetwork' :: Gene -> LinkMap -> LinkMap
      buildNetwork' (Gene (inNode, outNode) weight _ _) linkMap
        = Map.insert outNode result linkMap
          where
            result = case Map.lookup outNode linkMap of
              Just incoming -> Map.insert inNode weight incoming
              Nothing       -> Map.insert inNode weight Map.empty
