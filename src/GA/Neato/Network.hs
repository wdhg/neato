module GA.Neato.Network where

import GA.Neato.Genome

type LinkMap
  = [(Int, [(Int, Double)])]

type Nodes
  = [Double]

data Network
  = Network IOCount LinkMap


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

sigma :: Double -> Double
sigma
  = undefined

setNode :: Int -> Double -> Nodes -> Nodes
setNode index value nodes
  = before ++ (value : after)
    where
      (before, _ : after)
        = splitAt index nodes

computeNode :: (Network, Nodes) -> Int -> (Network, Nodes)
computeNode (network@(Network (inNodes, outNodes) links), nodes) node
  = case lookup node links of
      Nothing            -> (network, nodes)
      Just incoming -> (network, setNode node value nodes')
        where
          nodes'
            = snd $ foldl computeNode (network, nodes) $ map fst incoming
          value
            = sum $ map (\(n, weight) -> sigma $ weight * (nodes !! n)) incoming

run :: Network -> [Double] -> [Double]
run network@(Network (inNodes, outNodes) links) inputs
  = take outNodes $ drop inNodes $ snd computedNodes
    where
      computedNodes
        = foldl computeNode (network, inputs ++ repeat 0) [inNodes..outNodes - 1]
