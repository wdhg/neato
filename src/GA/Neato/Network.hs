module GA.Neato.Network where

type LinkMap
  = [[Int]]

type Nodes
  = [Double]

data Network
  = Network Nodes LinkMap
