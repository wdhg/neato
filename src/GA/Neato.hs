module GA.Neato where

import Data.Maybe (listToMaybe)

type Node
  = Int

type GeneID
  = Int

data Gene
  = Gene Node Node Double Bool GeneID

type GenePool
  = [Gene]

data Genome
  = Genome [Gene]
    deriving Eq

instance Eq Gene where
  (Gene _ _ _ _ geneID1) == (Gene _ _ _ _ geneID2)
    = geneID1 == geneID2

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

isGene :: GeneID -> Gene -> Bool
isGene geneID (Gene _ _ _ _ geneID')
  = geneID == geneID'

getGene :: Genome -> GeneID -> Maybe Gene
getGene (Genome genes) geneID
  = listToMaybe $ dropWhile (not . isGene geneID) genes
