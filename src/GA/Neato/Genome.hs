module GA.Neato.Genome where

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
