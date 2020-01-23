module GA.Neato.Genome where

import Data.List     (sort)
import System.Random

type Node
  = Int

type GeneID
  = Int

type Weight
  = Double

type State
  = Bool

data Gene
  = Gene Node Node Weight State GeneID

type Aligned
  = (Maybe Gene, Maybe Gene)

type GenePool
  = [Gene]

newtype Genome
  = Genome [Gene]
    deriving Eq

instance Eq Gene where
  (Gene _ _ _ _ geneID1) == (Gene _ _ _ _ geneID2)
    = geneID1 == geneID2

instance Ord Gene where
  (Gene _ _ _ _ geneID1) <= (Gene _ _ _ _ geneID2)
    = geneID1 <= geneID2

instance Show Gene where
  show (Gene inNode outNode weight state innovation)
    = unwords
      [ "GENE"
      , "[" ++ show innovation ++ "]"
      , show inNode, "-->", show outNode
      , "(" ++ show weight
      , show state ++ ")"
      ]

instance Show Genome where
  show (Genome genes)
    = unlines $ map show genes

alignGenes :: Genome -> Genome -> [Aligned]
alignGenes (Genome genes1) (Genome genes2)
  = alignGenes' (sort genes1) (sort genes2)
    where
      left, right :: Gene -> Aligned
      left gene  = (Just gene, Nothing)
      right gene = (Nothing, Just gene)
      alignGenes' :: [Gene] -> [Gene] -> [Aligned]
      alignGenes' [] gs2
        = map right gs2
      alignGenes' gs1' []
        = map left gs1'
      alignGenes' gs1@(g1 : gs1') gs2@(g2 : gs2')
        | g1 < g2   = left g1 : alignGenes' gs1' gs2
        | g1 > g2   = right g2 : alignGenes' gs1 gs2'
        | otherwise = (Just g1, Just g2) : alignGenes' gs1' gs2'

isSymmetric :: Aligned -> Bool
isSymmetric (Just _, Just _)
  = True
isSymmetric _
  = False

calcMeanWeightDelta :: [Aligned] -> Double
calcMeanWeightDelta alignment
  | null symmetric = 0.0
  | otherwise = (sum $ map calc symmetric) / (fromIntegral $ length symmetric)
    where
      symmetric = filter isSymmetric alignment
      calc :: Aligned -> Double
      calc (Just (Gene _ _ weight1 _ _), Just (Gene _ _ weight2 _ _))
        = abs $ weight1 - weight2


countDisjointExcess :: [Aligned] -> (Int, Int)
countDisjointExcess alignment
  = (length $ filter (not . isSymmetric) disjoint, length excess)
    where
      (disjoint, excess)
        = until isExcess pop ([], alignment)
      pop :: ([Aligned], [Aligned]) -> ([Aligned], [Aligned])
      pop (left, item : right)
        = (item : left, right)
      isExcess :: ([Aligned], [Aligned]) -> Bool
      isExcess (_, [])
        = True
      isExcess (_, alignment')
        = (check fst) || (check snd)
          where
            check func
              = all ((== Nothing) . func) alignment'


distance :: (Double, Double, Double) -> Genome -> Genome -> Double
distance (c1, c2, c3) genome1@(Genome genes1) genome2@(Genome genes2)
  = meanWeightDelta + (c1 * disjoint' + c2 * excess') / n
    where
      n = fromIntegral $ max (length genes1) (length genes2)
      alignment
        = alignGenes genome1 genome2
      meanWeightDelta
        = c3 * calcMeanWeightDelta alignment
      (disjoint, excess)
        = countDisjointExcess alignment
      disjoint'
        = fromIntegral disjoint
      excess'
        = fromIntegral excess

-- pick a gene
-- disable it
-- create two new genes in and out of a new node
-- add it to genome and genepool
mutateNode :: RandomGen g => g -> (Genome, GenePool) -> ((Genome, GenePool), g)
mutateNode gen genome
  = undefined

-- pick two unlinked nodes
-- create new gene between nodes
-- add to genome and genepool
mutateLink :: RandomGen g => g -> (Genome, GenePool) -> ((Genome, GenePool), g)
mutateLink gen genome
  = undefined

-- map over each gene
--   maybe change weight
--   maybe change state
mutateGenes :: RandomGen g => g -> Genome -> (Genome, g)
mutateGenes gen (Genome genes)
  = undefined

mutate :: RandomGen g => g -> Genome -> (Genome, g)
mutate gen genome
  = undefined
