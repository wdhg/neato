module GA.Neato.Network where

import GA.Neato.Genome

type LinkMap
  = [[Int]]

type Nodes
  = [Double]

data Network
  = Network IOCount Nodes LinkMap
