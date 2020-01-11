module Neato where

type Neuron
  = Int

-- Gene encodes a link between two neurons, (out, in)
type Gene
  = (Neuron, Neuron)

-- GeneHistory keeps track of genes as they appear in the population
type GeneHistory
  = [Gene]

type GeneWrapper
  = (Gene, Float, Bool)

type Genome
  = [GeneWrapper]

type Generation
  = ([Genome], GeneHistory)

type Simulation
  = [Generation]
