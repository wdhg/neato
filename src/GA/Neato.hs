module GA.Neato where

type Node
  = Int

type ID
  = Int

data Gene
  = Gene Node Node Double Bool ID

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

getGene :: Genome -> Gene -> Maybe Gene
getGene (Genome genes) gene
  | gene `elem` genes = Just $ head $ dropWhile (/= gene) genes
  | otherwise         = Nothing
