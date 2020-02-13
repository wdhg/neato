module GA.Neato.Network where

import GA.Neato.Genome

type LinkMap
  = [[Double]]

type Nodes
  = [Double]

data Network
  = Network IOCount LinkMap
