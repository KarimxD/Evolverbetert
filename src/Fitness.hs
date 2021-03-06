{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Fitness where
import Types
import qualified Parameters as P
import qualified Data.Map as Map
import Misc
import World (updateAgent)

checkAgentFitness :: Env -> Agent -> Double
checkAgentFitness _  NoAgent = 0
checkAgentFitness e  a =
    Map.findWithDefault
        (fitness e a) -- If not in map calculate
        e
        (agentFitness a)

class HammDist a => HasFitness a where
    fitness :: Env -> a -> Double
    fitness e a = (1 - d / dmax)^p
        where
            p = P.selectionPressure
            dmax = fromIntegral P.nrGeneTypes
            d = fromIntegral $ hammDist e a
instance HasFitness Agent where
    -- | The fitness of an Agent in an Environment
    fitness _  NoAgent = 0
    fitness e  a       = --fitness e $ toGST a
        if not $ hasCyclicAttractor a
            then fitness e $ toGST a
            else avergageOfFive e fitness a

avergageOfFive :: (Real a, Fractional b) => Env -> (Env -> GST -> a) -> Agent -> b
avergageOfFive e fitnessOrHammDist = average . map (fitnessOrHammDist e . toGST) . take 5 . iterate updateAgent

instance HasFitness Genome
instance HasFitness Chromosome
instance HasFitness GST

class InferGST a => HammDist a where
    hammDist :: Env -> a -> Int
    hammDist e x = hammingDistance (Map.elems $ targetGST e) list
        where list = map toOnOff $ Map.elems $ toGST x
instance HammDist Agent where
    hammDist _ NoAgent = P.nrGeneTypes'
    hammDist e a =
        if not $ hasCyclicAttractor a
            then hammDist e $ geneStateTable a
            else ceiling (avergageOfFive e hammDist a :: Double)
instance HammDist Genome
instance HammDist Chromosome
instance HammDist GST

-- | Calculate Hamming distance between two lists. For lists with unequal
-- lengths compares only the initial overlap
hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = length $ filter (==True) $ zipWith (/=) xs (take (P.nrHouseHold + P.nrOverlap + P.nrSpecific) ys)

-- | Generate GeneStateTable based on targetExpression
-- targetGST 0 = Map.fromList $ valueResultPairs (targetExpression 0) [0..P.nrFitEffect-1]
-- targetGST 1 = Map.fromList $ valueResultPairs (targetExpression 1) [0..P.nrFitEffect-1]
-- targetGST e = Map.fromList $
--     take P.nrFitEffect' $ valueResultPairs (targetExpression e) [0..]

--TODO: this is just for analysis code
targetGST :: Env -> GST
targetGST 0 = Map.fromList $ zip [0..] [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0]
targetGST 1 = Map.fromList $ zip [0..] [1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1]
targetGST _ = error "target not defined"

{- | the targetExpression of a Gene in an Environment
Considers all genes as Specific when the ID is bigger then nrHouseHold + nrOverlap
example for nrEnv = 4 and nrHouseHold = 4, nrOverlap = 3, nrSpecific = 5
Env\Gene    0   1   2   3   4   5   6   7   8   9   10  11
0           1   1   1   1   0   1   1   1   0   0   0   1
1           1   1   1   1   1   0   1   0   1   0   0   0
2           1   1   1   1   1   1   0   0   0   1   0   0
3           1   1   1   1   0   1   1   0   0   0   1   0
-}
targetExpression :: Env -> ID -> GeneState
targetExpression e i'
    | i <  hh                              = 1 -- household

    | i < hh + ov &&
        (i - hh - e) `mod` P.nrEnv == 0    = 0
    | i < hh + ov &&
        (i - hh - e) `mod` P.nrEnv /= 0    = 1 -- overlapping

    | (i - hh - ov - e) `mod` P.nrEnv == 0 = 1
    | otherwise                            = 0 -- specific
    where hh = P.nrHouseHold; ov = P.nrOverlap; i = (\(ID a) -> a) i'
