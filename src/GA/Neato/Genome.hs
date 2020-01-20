module GA.Neato.Genome where

import Data.List (sort)

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

type Alignment
  = (Maybe Gene, Maybe Gene)

type GenePool
  = [Gene]

data Genome
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

alignGenes :: Genome -> Genome -> [Alignment]
alignGenes (Genome genes1) (Genome genes2)
  = alignGenes' (sort genes1) (sort genes2)
    where
      left, right :: Gene -> Alignment
      left gene  = (Just gene, Nothing)
      right gene = (Nothing, Just gene)
      alignGenes' :: [Gene] -> [Gene] -> [Alignment]
      alignGenes' [] gs2
        = map right gs2
      alignGenes' gs1' []
        = map left gs1'
      alignGenes' gs1@(g1 : gs1') gs2@(g2 : gs2')
        | g1 < g2   = left g1 : alignGenes' gs1' gs2
        | g1 > g2   = right g2 : alignGenes' gs1 gs2'
        | otherwise = (Just g1, Just g2) : alignGenes' gs1' gs2'

distance :: (Double, Double, Double) -> Genome -> Genome -> Double
distance (c1, c2, c3) (Genome genes1) (Genome genes2)
  = c3 * meanWeightDelta + (c1 * disjoint + c2 * excess) / n
    where
      n = fromIntegral $ max (length genes1) (length genes2)
      meanWeightDelta
        = undefined
      disjoint
        = undefined
      excess
        = undefined
