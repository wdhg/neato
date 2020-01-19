module GA.Neato.Population where

import GA.Neato.Genome
import GA.Neato.Species

data Population
  = Population [Species]
