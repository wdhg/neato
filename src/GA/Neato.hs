module GA.Neato where

type Node
  = Int

data Gene
  = Gene Node Node Double Bool Int

type GenePool
  = [Gene]

data Genome
  = Genome [Gene]
    deriving Eq

instance Eq Gene where
  (Gene in1 out1 _ _ _) == (Gene in2 out2 _ _ _)
    = in1 == in2 && out1 == out2

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
