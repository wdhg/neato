module GA.Neato.Genome where

import Data.List     (sort)
import Data.Maybe    (maybeToList)
import System.Random

type Node
  = Int

type Link
  = (Node, Node)

type Weight
  = Double

type State
  = Bool

type GeneID
  = Int

data Gene
  = Gene Link Weight State GeneID

type Aligned
  = (Maybe Gene, Maybe Gene)

type GenePool
  = [(Link, GeneID)]

type IOCount
  = (Int, Int)

data Genome
  = Genome IOCount [Gene]
    deriving Eq

type Mutation g
  = (g, GenePool, Genome) -> (g, GenePool, Genome)

instance Eq Gene where
  (Gene _ _ _ geneID1) == (Gene _ _ _ geneID2)
    = geneID1 == geneID2

instance Ord Gene where
  (Gene _ _ _ geneID1) <= (Gene _ _ _ geneID2)
    = geneID1 <= geneID2

instance Show Gene where
  show (Gene (inNode, outNode) weight state innovation)
    = unwords
      [ "GENE"
      , "[" ++ show innovation ++ "]"
      , show inNode, "-->", show outNode
      , "(" ++ show weight
      , show state ++ ")"
      ]

instance Show Genome where
  show (Genome _ genes)
    = unlines $ map show genes

alignGenes :: Genome -> Genome -> [Aligned]
alignGenes (Genome _ genes1) (Genome _ genes2)
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
      calc (Just (Gene _ weight1 _ _), Just (Gene _ weight2 _ _))
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
distance (c1, c2, c3) genome1@(Genome _ genes1) genome2@(Genome _ genes2)
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

perturbWeight :: RandomGen g => g -> Weight -> (Weight, g)
perturbWeight gen weight
  = (weight + (value * delta * 2 - delta), gen')
    where
      delta         = 0.1
      value :: Double
      (value, gen') = randomR (0, 1) gen

newWeight :: RandomGen g => g -> (Weight, g)
newWeight gen
  = (value * maxWeight * 2 - maxWeight, gen')
    where
      maxWeight     = 2
      value :: Double
      (value, gen') = randomR (0, 1) gen

mutateWeight :: RandomGen g => g -> Weight -> (Weight, g)
mutateWeight gen weight
  | value < perturbChance = perturbWeight gen' weight
  | otherwise             = newWeight gen'
    where
      perturbChance = 0.9
      value :: Double
      (value, gen') = randomR (0, 1) gen

-- map over each gene
--   maybe change weight
--   maybe change state
mutateGenes :: RandomGen g => Mutation g
-- Pre: non-empty Genome
mutateGenes (gen, pool, Genome io (gene : genes))
  = (snd $ last mutations, pool, Genome io $ map fst mutations)
    where
      mutations
        = scanl (\(_, gen') gene' -> mutateGene gen' gene') (gene, gen) genes
      mutateGene :: RandomGen g => g -> Gene -> (Gene, g)
      mutateGene gen' (Gene link weight state geneID)
        = (Gene link weight' state geneID, gene'')
          where
            (weight', gene'') = mutateWeight gen' weight

getNodes :: [Gene] -> [Node]
getNodes []
  = []
getNodes ((Gene (inNode, outNode) _ _ _) : genes)
  = sort $ push inNode $ push outNode $ getNodes genes
    where
      push :: Node -> [Node] -> [Node]
      push node nodes
        | node `elem` nodes = nodes
        | otherwise         = node : nodes


getNextNode :: Genome -> Node
getNextNode (Genome _ genes)
  = head $ dropWhile (`elem` nodes) [0..]
    where
      nodes
        = getNodes genes

getGeneID :: GenePool -> Link -> (GeneID, GenePool)
getGeneID pool link
  = case lookup link pool of
      Just geneID -> (geneID, pool)
      Nothing     -> (geneID', pool')
    where
      geneID'
        = length pool
      pool'
        = (link, geneID') : pool

-- pick a gene
-- disable it
-- create two new genes in and out of a new node
-- add it to genome and genepool
mutateNode :: RandomGen g => Mutation g
mutateNode (gen, pool, genome@(Genome io genes))
  = (gen', pool'', Genome io $ before ++ (gene : geneIn : geneOut : after))
    where
      -- -2 from length so pattern always matches
      index :: Int
      (index, gen') = randomR (0, length genes - 2) gen
      (before, ((Gene (inNode, outNode) weight state geneID) : after))
        = splitAt index genes
      newNode             = getNextNode genome
      (geneInID, pool')   = getGeneID pool (inNode, newNode)
      geneIn              = Gene (inNode, newNode) weight state geneInID
      (geneOutID, pool'') = getGeneID pool' (newNode, outNode)
      geneOut             = Gene (newNode, outNode) 1.0 state geneOutID
      gene                = Gene (inNode, outNode) weight False geneID

getUnlinked :: Genome -> [Link]
getUnlinked (Genome (inputCount, outputCount) genes)
  = filter (\link -> notLinked link && notCyclic link) links
    where
      nodes      = getNodes genes
      (inputs, nonInputs)
        = splitAt inputCount nodes
      (outputs, hidden)
        = splitAt outputCount nonInputs
      links
        = [(inNode, outNode) | inNode <- (inputs ++ hidden), outNode <- (outputs ++ hidden)]
      presentLinks = map (\(Gene link _ _ _) -> link) genes
      getLinked :: Node -> [Node]
      getLinked node
        = concatMap getLinked $ map fst $ filter ((== node) . snd) presentLinks
      notLinked :: Link -> Bool
      notLinked link@(inNode, outNode)
        = link `notElem` presentLinks && outNode `notElem` (getLinked inNode)
      notCyclic :: Link -> Bool
      notCyclic (inNode, outNode)
        | inNode == outNode = False
        | otherwise         = notLinked (outNode, inNode)

-- pick two unlinked nodes
-- create new gene between nodes
-- add to genome and genepool
mutateLink :: RandomGen g => Mutation g
mutateLink (gen, pool, genome@(Genome io genes))
  = (gen'', pool', Genome io (gene : genes))
    where
      links
        = getUnlinked genome
      (index, gen')
        = randomR (0, length links - 1) gen
      link
        = links !! index
      (weight, gen'')
        = (random gen')
      (geneID, pool')
        = getGeneID pool link
      gene
        = Gene link (weight * 4 - 2) True geneID

chanceMutate :: RandomGen g => Double -> Mutation g -> (g, GenePool, Genome) -> (g, GenePool, Genome)
chanceMutate chance mutation (gen, pool, genome)
  | value < chance = mutation (gen', pool, genome)
  | otherwise      = (gen', pool, genome)
    where
      (value, gen')
        = randomR (0, 1) gen

mutate :: RandomGen g => Mutation g
mutate
  = chanceMutate 0.03 mutateNode . chanceMutate 0.05 mutateLink . mutateGenes

-- breed assumes genome1 is more fit than genome2
breed :: RandomGen g => g -> Genome -> Genome -> (Genome, g)
breed gen genome1@(Genome io genes1) genome2
  = (Genome io $ concatMap (maybeToList . fst) inheritance, snd $ last inheritance)
    where
      alignment
        = alignGenes genome1 genome2
      inheritance
        = scanl (\(_, gen') aligned -> inherit gen' aligned) (Nothing, gen) alignment
      inherit :: RandomGen g => g -> Aligned -> (Maybe Gene, g)
      inherit gen (left, Nothing)
        = (left, gen)
      inherit gen (Nothing, _)
        = (Nothing, gen)
      inherit gen (Just (Gene link weight1 enabled1 geneID), Just (Gene _ weight2 enabled2 _))
        = (Just $ Gene link weight' enabled' geneID, gen'')
          where
            inheritLeft :: Bool
            (inheritLeft, gen')
              = random gen
            value :: Double
            (value, gen'')
              = randomR (0, 1) gen'
            weight'
              | inheritLeft = weight1
              | otherwise   = weight2
            enabled'
              | enabled1 && enabled2 = True
              | otherwise            = value < 0.25
