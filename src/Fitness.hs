{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Fitness where
import Types
import qualified Parameters as P
import qualified Data.Map as Map
import Misc

class HasFitness a where
    fitness :: Env -> a -> Double

instance HasFitness Agent where
    -- | The fitness of an Agent in an Environment (stub for 'fitnessGST')
    fitness _  NoAgent      = 0
    fitness e  a = fitness e $ toGST a
instance HasFitness Genome where
    fitness e = fitness e . concat
instance HasFitness Chromosome where
    fitness e = fitness e . toGST
instance HasFitness GeneStateTable where
    -- | Uses targetGST to check fitness of passed GST
    fitness e gst = (1 - d / dmax)^p
            where
                p = P.selectionPressure
                dmax = fromIntegral P.nrGeneTypes
                d = fromIntegral $ hammingDistance target this
                    where target = Map.toList (targetGST e)
                          this = Map.toList gst

class HammDist a where
    hammDist :: Env -> a -> Int
instance HammDist Agent where
    hammDist _ NoAgent = fromIntegral P.nrFitEffect
    hammDist e ag = hammDist e $ geneStateTable ag
instance HammDist Genome where
    hammDist e = hammDist e . concat
instance HammDist Chromosome where
    hammDist e = hammDist e . toGST
instance HammDist GeneStateTable where
    hammDist e = hammingDistance (Map.toList $ targetGST e) . Map.toList

-- | Calculate Hamming distance between two lists. For lists with unequal
-- lengths compares only the initial overlap
hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = length $ filter (==True) $ zipWith (/=) xs (take (P.nrHouseHold + P.nrOverlap + P.nrSpecific) ys)

-- | Generate GeneStateTable based on targetExpression
targetGST :: Env -> GeneStateTable
targetGST 0 = Map.fromList $ valueResultPairs (targetExpression 0) [0..P.nrFitEffect-1]
targetGST 1 = Map.fromList $ valueResultPairs (targetExpression 1) [0..P.nrFitEffect-1]
targetGST e = Map.fromList $
    take P.nrFitEffect' $ valueResultPairs (targetExpression e) [0..]





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
