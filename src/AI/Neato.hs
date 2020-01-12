module AI.Neato where

type Neuron
  = Int

type Gene
  = (Neuron, Neuron)

type GenePool
  = [Gene]

-- represents what genes in the genePool are present in the genome via its bits
type GeneVector
  = Integer

data Genome
  = Genome GeneVector [Double] [Bool]
    deriving (Eq, Show)

getYoungestGene :: Genome -> GeneVector
getYoungestGene (Genome genes _ _)
  = (head $ dropWhile ((<= genes) . (2 ^)) [0..]) - 1
