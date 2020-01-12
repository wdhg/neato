module AI.Neato where

import Data.Bits

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

getYoungestGene :: GeneVector -> Int
getYoungestGene genes
  = (head $ dropWhile ((<= genes) . (2 ^)) [0..]) - 1

countExcess :: Genome -> Genome -> Int
countExcess (Genome genes1 _ _) (Genome genes2 _ _)
  = popCount $ (shiftR genes1 end) `xor` (shiftR genes2 end)
    where
      end = succ $ min (getYoungestGene genes1) (getYoungestGene genes2)

countDisjoint :: Genome -> Genome -> Int
countDisjoint (Genome genes1 _ _) (Genome genes2 _ _)
  = popCount $ (genes1 .&. mask) `xor` (genes2 .&. mask)
    where
      end  = succ $ min (getYoungestGene genes1) (getYoungestGene genes2)
      mask = pred $ fromIntegral $ popCount $ succ end
