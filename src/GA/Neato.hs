module GA.Neato where

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

getIndices :: GeneVector -> [Int]
getIndices genes
  = filter (testBit genes) $ [0..fromIntegral genes]

getWeight :: Genome -> Int -> Double
getWeight (Genome genes weights _) gene
  = weights !! index
    where
      index = popCount $ genes .&. (bit gene - 1)

getYoungestGene :: GeneVector -> Int
getYoungestGene genes
  = maximum $ getIndices genes

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
      mask = bit end - 1

calcMeanWeightDelta :: Genome -> Genome -> Double
calcMeanWeightDelta genome1@(Genome genes1 _ _) genome2@(Genome genes2 _ _)
  = weightDeltaSum / (fromIntegral $ popCount sharedGenes)
    where
      sharedGenes = fromInteger $ genes1 .&. genes2
      geneIndices = filter (testBit sharedGenes) [0..sharedGenes]
      weightDeltaSum   = sum $ map calcWeightDelta geneIndices
      calcWeightDelta :: Int -> Double
      calcWeightDelta i
        = abs (getWeight genome1 i - getWeight genome2 i)

distance :: (Double, Double, Double) -> Genome -> Genome -> Double
distance (c1, c2, c3) genome1@(Genome genes1 _ _) genome2@(Genome genes2 _ _)
  = c3 * meanWeightDelta + (c1 * excess + c2 * disjoint) / n
    where
      excess          = fromIntegral $ countExcess genome1 genome2
      disjoint        = fromIntegral $ countExcess genome1 genome2
      meanWeightDelta = calcMeanWeightDelta genome1 genome2
      n
        | n' < 20   = 1
        | otherwise = n'
          where
            n' = fromIntegral $ max (popCount genes1) (popCount genes2)

getGenes :: GenePool -> Genome -> [Gene]
getGenes genePool (Genome genes _ _)
  = map (genePool !!) $ getIndices genes

geneIndex :: GenePool -> Gene -> Maybe Int
geneIndex pool gene
  | gene `elem` pool = Just $ length $ takeWhile (/= gene) pool
  | otherwise        = Nothing
