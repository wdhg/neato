module GA.Neato where

import Data.List (sort)

type Node
  = Int

type GeneID
  = Int

data Gene
  = Gene Node Node Double Bool GeneID

type GeneAlignment
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
  show (Gene inNode outNode weight active innovation)
    = unwords
      [ "GENE"
      , "[" ++ show innovation ++ "]"
      , show inNode, "-->", show outNode
      , "(" ++ show weight
      , show active ++ ")"
      ]

instance Show Genome where
  show (Genome genes)
    = unlines $ map show genes

alignGenes :: Genome -> Genome -> GeneAlignment
alignGenes (Genome genes1) (Genome genes2)
  = alignGenes' (sort genes1) (sort genes2)
    where
      left, right :: Gene -> (Maybe Gene, Maybe Gene)
      left gene  = (Just gene, Nothing)
      right gene = (Nothing, Just gene)
      alignGenes' :: [Gene] -> [Gene] -> [(Maybe Gene, Maybe Gene)]
      alignGenes' [] genes2'
        = map right genes2'
      alignGenes' genes1' []
        = map left genes1'
      alignGenes' (gene1 : genes1') (gene2 : genes2')
        | gene1 < gene2 = left gene1 : alignGenes' genes1' (gene2 : genes2')
        | gene1 > gene2 = right gene2 : alignGenes' (gene1 : genes1') genes2'
        | otherwise     = (Just gene1, Just gene2) : alignGenes' genes1' genes2'
