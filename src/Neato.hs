module Neato where

type Neuron
  = Int

-- Gene encodes a link between two neurons, (out, in)
type Gene
  = (Neuron, Neuron)

-- GeneHistory keeps track of genes as they appear in the population
type GeneHistory
  = [Gene]

type Genome
  = [(Gene, Float, Bool)]

type Generation
  = [Genome]
