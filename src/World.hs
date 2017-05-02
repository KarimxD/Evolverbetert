{-# LANGUAGE BangPatterns #-}

module World
-- (
    -- World, Env, Agents, Agent(..), Genome, Chromosome, GeneStateTable, Locus(..), Gene(..), Tfbs(..), ID, Thres, GeneState
    -- , devAg, agent0, groupGeneTfbs, gSTFromGenome, fitnessAgent, showGST, hammDist, targetGST, hammDistAg)
    where
import Control.Monad
import qualified Data.List         as List
import qualified Data.Map.Strict          as Map
import           Data.Array.IArray
import           Misc
import qualified Parameters        as P
import MyRandom
import Types

import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Control.Monad.State (state)
import Data.Maybe (mapMaybe)
import           Data.List.Split     (splitOn)






-- | Develops agents according to 'P.devTime' and 'updateAgent'
devAg :: Agent -> Agent
devAg = takeUntilSame . take P.devTime . iterate updateAgent
    where
    takeUntilSame [a,b] = NoAgent
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

-- | Updates the genestates in the genome and the genestatetable with
-- 'updateGenome' and 'gSTFromGenome'
-- Kills the agent if it doesn't have all genes (when length gst /= 'P.nrGeneTypes')
updateAgent :: Agent -> Agent
updateAgent NoAgent = NoAgent
updateAgent (Agent chroms gst) =
    if length newGST == P.nrGeneTypes'
        then Agent newGenome newGST
        else NoAgent
    where newGenome = updateGenome gst chroms
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
updateLoc a _ loc@(CGene gen@(Gene i t st)) =
    (0, CGene (Gene i t newState)) where
        newState    | fromIntegral a <  fromIntegral t = GS False
                    | fromIntegral a == fromIntegral t = st
                    | otherwise                        = GS True
updateLoc a _ loc = (a, loc)

-- | Check whether a locus is a Gene
isGene, isTfbs :: Locus -> Bool
isGene (CGene _) = True
isGene _         = False
isTfbs (CTfbs _) = True
isTfbs _         = False

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
takeWhileInclusive f ls = takeWhileInclusive' f ([], ls)
    where
    takeWhileInclusive' :: (a -> Bool) -> ([a],[a]) -> ([a],[a])
    takeWhileInclusive' _ (a,[]) = (a,[])
    takeWhileInclusive' f (a,x:xs) =
        if f x
            then (henk, xs)
            else takeWhileInclusive' f (henk,xs)
                where henk = a ++ [x]
                --
                -- > foldr f z []     = z
                -- > foldr f z (x:xs) = x `f` foldr f z xs

-- | Generate GST from a genome
gSTFromGenome :: Genome -> GeneStateTable
gSTFromGenome = makeGST . reduceToGenes --makeGst Map.empty $ reduceToGenes genes
    where
    makeGST :: [Gene] -> GeneStateTable
    makeGST = List.foldl'
        (\acc x -> Map.insertWith max (iD x) (genSt x) acc)
        Map.empty

-- | Reduce a genome to a list of its transcription factor binding sites
reduceToTfbss :: Genome -> [Tfbs]
reduceToTfbss = mapMaybe getTfbs . concat

-- | Reduce a genome to a list of its genes
reduceToGenes :: Genome -> [Gene]
reduceToGenes = mapMaybe getGene . concat

-- | The fitness of an Agent in an Environment (stub for 'fitnessGST')
fitnessAgent :: Env -> Agent -> Double
fitnessAgent e (Agent _ gst) = fitnessGST e gst
fitnessAgent e  NoAgent      = 0

-- | Uses targetGST to check fitness of passed GST
fitnessGST :: Env -> GeneStateTable -> Double
fitnessGST env gst = (1 - d / dmax)^p
    where
        p = P.selectionPressure
        dmax = fromIntegral P.nrGeneTypes
        d = fromIntegral $ hammDist target this
            where target = Map.toList (targetGST env)
                  this = Map.toList gst

-- | Calculate Hamming distance between two lists. For lists with unequal
-- lengths compares only the initial overlap
hammDist :: (Eq a) => [a] -> [a] -> Int
-- hammDist = ((length . filter (True ==)) .) . zipWith (/=)
hammDist [] _ = 0
hammDist _ [] = 0
hammDist (a:as) (b:bs) = if a /= b then 1 + hammDist as bs else hammDist as bs
-- {-# SPECIALIZE hammDist :: [(Int,Int)] -> [(Int,Int)] -> Int #-}

hammDistAg :: Env -> Agent -> Int
hammDistAg _ NoAgent = fromIntegral P.nrGeneTypes
hammDistAg env ag = hammDist (Map.toList (targetGST env)) (Map.toList $ geneStateTable ag)

-- | Generate GeneStateTable based on targetExpression
targetGST :: Env -> GeneStateTable
targetGST 0 = Map.fromList $ valueResultPairs (targetExpression 0) [0..P.nrGeneTypes-1]
targetGST 1 = Map.fromList $ valueResultPairs (targetExpression 1) [0..P.nrGeneTypes-1]
targetGST env = Map.fromList $
    take P.nrGeneTypes' $ valueResultPairs (targetExpression env) [0..]

{- | the targetExpression of a Gene in an Environment
example for nrEnv = 4 and nrHouseHold = 4, nrOverlap = 3, nrUnique = 5
Env\Gene    0   1   2   3   4   5   6   7   8   9   10  11
0           1   1   1   1   0   1   1   1   0   0   0   1
1           1   1   1   1   1   0   1   0   1   0   0   0
2           1   1   1   1   1   1   0   0   0   1   0   0
3           1   1   1   1   0   1   1   0   0   0   1   0
-}
targetExpression :: Env -> ID -> GeneState
targetExpression env i'
    | i <  hh                                   = 1 -- household

    | i < hh + ov &&
        (i - hh - env) `mod` P.nrEnv == 0       = 0
    | i < hh + ov &&
        (i - hh - env) `mod` P.nrEnv /= 0       = 1 -- overlapping

    | (i - hh - ov - env) `mod` P.nrEnv == 0    = 1
    | otherwise                                 = 0 -- specific
    where hh = P.nrHouseHold; ov = P.nrOverlap; sp = P.nrSpecific; i = (\(ID a) -> a) i'













-- | Generate a random Agent using 'goodRandomGenome'
randomAgent :: Rand Agent
randomAgent = do
    genome <- goodRandomGenome
    let  agent = devAg $ Agent genome defaultGst
    if   agent == NoAgent
        then randomAgent
        else return agent

-- | Does every gene in genome have an associated transcription factor?
connected :: Genome -> Bool
connected = all (>1) . map length . groupGeneTfbs . concat

-- | Generate a random genome that is 'connected'
goodRandomGenome :: Rand Genome
goodRandomGenome = do
    genome <- randomGenome
    if      connected genome
       then return genome
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
    randomWeights <- replicateM n' $ state randomW

    r <- getModifyRand
    let shuffled = shuffle' randomWeights n' r
    return $ map CTfbs $ shuffle' (zipWith Tfbs [0..n-1] shuffled) n' r
        where
            n' = P.nrGeneTypes'; n = P.nrGeneTypes
            randomW :: PureMT -> (Weight, PureMT)
            randomW g = let (d, g') = randomBool g
                        in if d then (-1, g') else (1, g')

-- | Generate all possible genes (0..nrGeneTypes) with each random threshold
-- and state 0
randomGenes :: Rand [Locus]
randomGenes = do
    randomThresholds <- replicateM n' $ fromIntegral <$> getRange (P.minThres, P.maxThres)
    r <- getModifyRand
    let shuffled = shuffle' randomThresholds n' r
    return $ map CGene $ shuffle' (zipWith makeGene [0..n-1] shuffled) n' r
        where n' = P.nrGeneTypes'; n = P.nrGeneTypes
              makeGene i t = Gene i t 0
