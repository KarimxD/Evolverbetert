{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module World
-- (
    -- World, Env, Agents, Agent(..), Genome, Chromosome, GeneStateTable, Locus(..), Gene(..), Tfbs(..), ID, Thres, GeneState
    -- , devAg, agent0, groupGeneTfbs, gSTFromGenome, fitnessAgent, showGST, hammDist, targetGST, hammDistAg)
    where
import Control.Monad
import qualified Data.Map.Strict          as Map
import Data.Foldable as F (foldr')
import Data.Maybe (isJust, mapMaybe)

import           Misc
import qualified Parameters        as P
import MyRandom
import Types

import System.Random.Shuffle (shuffle')
import Control.Monad.State (state)






-- | Develops agents according to 'P.devTime' and 'updateAgent'
devAg :: Agent -> Agent
devAg = if   P.resetGeneStatesOnBirth
        then devAg' . setToStart
        else devAg'

devAg' :: Agent -> Agent
devAg' = takeUntilSame . take P.devTime . iterate updateAgent
    where
        takeUntilSame [_,_] = NoAgent; takeUntilSame [_] = NoAgent; takeUntilSame [] = NoAgent
        takeUntilSame (a:b:rest) =
            if sameGST a b
                then   a
                else   takeUntilSame $ b:rest

-- | Do two agents share GST
sameGST :: Agent -> Agent -> Bool
sameGST NoAgent _ = True
sameGST _ NoAgent = False
sameGST ag1 ag2 = geneStateTable ag1 == geneStateTable ag2

-- | The default GeneStateTable where All genes have expression 0
defaultGst :: GeneStateTable
defaultGst = Map.fromList defaultStates
    where defaultStates = take P.nrGeneTypes' $ zip [0..] (repeat 0) :: [(ID,GeneState)]

-- | Check if locus is effected by expressed transcription factors
-- Return the effect it has. 0 is no effect.
locusEffect :: Locus -> GeneStateTable -> Weight
locusEffect (CTfbs (Tfbs i w)) gst
    | Map.lookup i gst == Just 1 = w
    | otherwise                  = 0
locusEffect _ _ = 0

setToStart :: Agent -> Agent
setToStart ag = ag { genome = updateGenome startingGST $ genome ag
                   , geneStateTable = startingGST }

-- | Updates the genestates in the genome and the genestatetable with
-- 'updateGenome' and 'gSTFromGenome'
-- Kills the agent if it doesn't have all genes (when length gst /= 'P.nrGeneTypes')
updateAgent :: Agent -> Agent
updateAgent NoAgent = NoAgent
updateAgent ag =
    if length newGST == P.nrGeneTypes'
        then ag { genome = newGenome, geneStateTable = newGST}
        else NoAgent
    where
          newGenome = updateGenome (geneStateTable ag) (genome ag)
          newGST = gSTFromGenome newGenome

-- | Updates every Chromosome in Genome with updateChrom
updateGenome :: GeneStateTable -> Genome -> Genome
updateGenome = map . updateChrom 0

-- | Updates each gene in Chromosome using 'updateLoc'
-- Initial argument is an accumulator that counts the effects of binding sites.
updateChrom :: Weight -> GeneStateTable -> Chromosome -> Chromosome
updateChrom a gst (l:ls) = l' : updateChrom a' gst ls
    where (a', l') = updateLoc a gst l
updateChrom _ _ _ = []

-- | if Tfbs, increment accumulater with effect
-- if Gene, change the expression according to accumulater and set accumulater to 0
-- returns a (accumulator, changed Locus) pair
updateLoc :: Weight -> GeneStateTable -> Locus -> (Weight, Locus)
updateLoc a gst loc@(CTfbs (Tfbs i w))
        | Map.lookup i gst == Just 1 = (a + w, loc)
        | otherwise                  = (a, loc)
updateLoc a _ (CGene (Gene i t st)) =
    (0, CGene (Gene i t newState)) where
        newState    | fromIntegral a <= t = GS 0 --Gene never stays the same: fix change <= to < uncomment next line
                    -- | fromIntegral a == t = st
                    | otherwise           = GS 1
updateLoc a _ loc = (a, loc)

-- | Check whether a locus is a Gene or Tfbs
isGene, isTfbs :: Locus -> Bool
isGene = isJust . getGene
isTfbs = isJust . getTfbs

-- | Returns Just Gene if Locus is a Gene else Nothing
getGene :: Locus -> Maybe Gene
getGene (CGene g) = Just g
getGene _         = Nothing
-- | Returns Just Tfbs if Locus is a Tfbs else Nothing
getTfbs :: Locus -> Maybe Tfbs
getTfbs (CTfbs t) = Just t
getTfbs _         = Nothing


-- | Groups genes and preceding transcription factors together
-- e.g. [TF, TF, Gene, TF, Gene, TF] -> [[TF, TF, Gene], [TF, Gene], [TF]]
groupGeneTfbs :: [Locus] -> [[Locus]]
groupGeneTfbs [] = []
groupGeneTfbs loci = h: groupGeneTfbs t
    where (h,t) = takeWhileInclusive isGene loci

-- | takes a predicate and returns a pair of lists with the first ending with
-- the element that satisfies the predicate
takeWhileInclusive :: (a -> Bool) -> [a] -> ([a],[a])
takeWhileInclusive f ls = takeWhileInclusive' ([], ls)
    where
    takeWhileInclusive' (a,[]) = (a,[])
    takeWhileInclusive' (a,x:xs) =
        if f x
            then (henk, xs)
            else takeWhileInclusive' (henk,xs)
                where henk = a ++ [x]
                --
                -- > foldr f z []     = z
                -- > foldr f z (x:xs) = x `f` foldr f z xs

gSTFromChrom :: Chromosome -> GeneStateTable
gSTFromChrom = makeGST . reduceChromToGenes
    where
        makeGST :: [Gene] -> GeneStateTable
        makeGST = F.foldr'
            (\ !x !acc -> Map.insertWith
                (if P.dosiseffect then (+) else max)
                (iD x) (genSt x) acc)
                    Map.empty


-- | Generate GST from a genome
gSTFromGenome :: Genome -> GeneStateTable
gSTFromGenome = gSTFromChrom . concat --makeGst Map.empty $ reduceToGenes genes

reduceChromToTfbss :: Chromosome -> [Tfbs]
reduceChromToTfbss = mapMaybe getTfbs

-- | Reduce a genome to a list of its transcription factor binding sites
reduceGenomeToTfbss :: Genome -> [Tfbs]
reduceGenomeToTfbss = reduceChromToTfbss . concat

reduceChromToGenes :: Chromosome -> [Gene]
reduceChromToGenes = mapMaybe getGene

-- | Reduce a genome to a list of its genes
reduceGenomeToGenes :: Genome -> [Gene]
reduceGenomeToGenes = reduceChromToGenes . concat

class HasFitness a where
    fitness :: Env -> a -> Double

instance HasFitness Agent where
    -- | The fitness of an Agent in an Environment (stub for 'fitnessGST')
    fitness e (Agent _ gst _ _ _ _) = fitness e gst
    fitness _  NoAgent      = 0
instance HasFitness Genome where
    fitness e = fitness e . concat
instance HasFitness Chromosome where
    fitness e = fitness e . gSTFromChrom
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
    hammDist e = hammDist e . gSTFromChrom
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

{- | startingGST lays in between the attractors of targetExpression.
    For instance nrEnv = 4 and nrHouseHold = 4, nrOverlap = 3, nrSpecific = 5
    Env\Gene    0   1   2   3   4   5   6   7   8   9   10  11
    0           1   1   1   1   0   1   1   1   0   0   0   1
    1           1   1   1   1   1   0   1   0   1   0   0   0
    2           1   1   1   1   1   1   0   0   0   1   0   0
    3           1   1   1   1   0   1   1   0   0   0   1   0
    start       1   1   0   0   1   1   0   1   1   1   0   0
-}
startingGST :: GeneStateTable
startingGST = Map.fromList $ zip [0..] $ fhsh hh ++ fhsh ov ++ fhsh sp ++ fhsh ne
    where fhsh x -- firsthalfsecondhalf
            | even x    = replicate (x `div` 2    ) 1 ++ replicate (x `div` 2) 0
            | otherwise = replicate (x `div` 2 + 1) 1 ++ replicate (x `div` 2) 0
          hh = P.nrHouseHold; ov = P.nrOverlap; sp = P.nrSpecific; ne = P.nrNoEffect


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



-- | Answers the question: Does every gene of this genome have at least one associated transcription factor?
connected :: Genome -> Bool
connected = all (>1) . map length . groupGeneTfbs . concat

-- | Generate a random Agent using 'goodRandomGenome'
randomAgent :: Rand Agent
randomAgent = do
    randGenome <- goodRandomGenome
    let  agent = devAg $ Agent randGenome defaultGst 0 0 NoAgent []
    if   agent == NoAgent
        then randomAgent
        else return agent


-- | Generate a random genome that is 'connected'
-- loops until it finds one
goodRandomGenome :: Rand Genome
goodRandomGenome = do
    randGenome <- randomGenome
    if      connected randGenome
       then return randGenome
       else goodRandomGenome

-- | Just a 'randomChromosome'
randomGenome :: Rand Genome
randomGenome = fmap (:[]) randomChromosome

-- | Generate a random chromosome by shuffling twice as many random Tfbss
-- as random genes. using 'randomTfbss' and 'randomGenes' and 'shuffle''
randomChromosome :: Rand Chromosome
randomChromosome = do
    r <- getModifyRand
    randomChrom <- concat <$> sequence [randomTfbss,randomTfbss,randomGenes]
    return $ shuffle' randomChrom (length randomChrom) r

-- | Generate all possible Tfbss (0..nrGeneTypes) with each random weight
randomTfbss :: Rand [Locus]
randomTfbss = do
    -- randomWeights <- replicateM n' $ state randomW
    randomWeights <- replicateM n' $ state randomW

    r <- getModifyRand
    let shuffled = shuffle' randomWeights n' r
    return $ map CTfbs $ shuffle' (zipWith Tfbs [0..n-1] shuffled) n' r
        where
            n' = P.nrGeneTypes'; n = P.nrGeneTypes

            -- randomW :: Rand Weight
            -- randomW = Rand $ \s -> case randomBool s of (w,s') -> R (f w) s'
            --     where f x = if x then -1 else 1

            randomW :: PureMT -> (Weight, PureMT)
            randomW g = let (d, g') = randomBool g
                        in if d then (-1, g') else (1, g')

-- | Generate all possible genes (0..nrGeneTypes) with each random threshold
-- and state 0
randomGenes :: Rand [Locus]
randomGenes = do
    randomThresholds <- replicateM n' $ getRange (P.minThres, P.maxThres)
    r <- getModifyRand
    let shuffled = shuffle' randomThresholds n' r
    return $ map CGene $ shuffle' (zipWith makeGene [0..n-1] shuffled) n' r
        where n' = P.nrGeneTypes'; n = P.nrGeneTypes
              makeGene i t = Gene i t 0

dead :: Agent -> Bool
dead = not . living

living :: Agent -> Bool
living = (/=) NoAgent
