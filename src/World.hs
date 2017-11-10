{-# LANGUAGE BangPatterns #-}
module World
-- (
    -- World, Env, Agents, Agent(..), Genome, Chromosome, GeneStateTable, Locus(..), Gene(..), Tfbs(..), ID, Thres, GeneState
    -- , devAg, agent0, groupGeneTfbs, gSTFromGenome, fitnessAgent, showGST, hammDist, targetGST, hammDistAg)
    where
import Control.Monad
import qualified Data.Map.Strict          as Map
import qualified Parameters        as P

import MyRandom
import Types
import Fitness (startingGST)

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
defaultGst :: GST
defaultGst = Map.fromList defaultStates
    where defaultStates = take P.nrGeneTypes' $ zip [0..] (repeat 0) :: [(ID,GeneState)]

-- -- | Check if locus is effected by expressed transcription factors
-- -- Return the effect it has. 0 is no effect.
-- locusEffect :: Locus -> GeneStateTable -> Weight
-- locusEffect (CTfbs (Tfbs i w st)) gst
--     | Map.lookup i gst == Just 1 = w
--     | otherwise                  = 0
-- locusEffect _ _ = 0

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
          newGST = toGST newGenome

-- | Updates every Chromosome in Genome with updateChrom
updateGenome :: InferGST gst => gst -> Genome -> Genome
updateGenome = map . updateChrom --0

(↞) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
(↞) = (.).(.)


updateChrom :: InferGST gst => gst -> Chromosome -> Chromosome
updateChrom = updateGenes ↞ updateTfbss

updateTfbss :: InferGST gst => gst -> Chromosome -> Chromosome
updateTfbss !gst' =
    map (onTfbs $ \t -> if   P.dosiseffect
                        then t {tfbsSt =          gst Map.! tfbsID t}
                        else t {tfbsSt = bottom $ gst Map.! tfbsID t}
        )
    where bottom gs | gs == GS 0 = GS 0
                    | gs >  GS 0 = GS 1
                    | otherwise  = error "negative GeneState"
          gst = toGST gst'

updateGenes :: Chromosome -> Chromosome
updateGenes = updateGenes' 0
    where updateGenes' :: Integer -> Chromosome -> Chromosome
          updateGenes' _ []     = []
          updateGenes' !a (l:ls) = case l of
              CTfbs t -> l : updateGenes' (a + s * w) ls
                where w = toInteger $ wt t; s = toInteger $ tfbsSt t
              _       -> onGene (updateGene a) l : updateGenes' 0 ls
            --   x       -> x : updateGenes' a ls

updateGene :: Integer -> Gene -> Gene
updateGene !a !g = g {genSt = newState} where
    newState    | a <=  t     = GS 0 --Gene never stays the same: fix change <= to < uncomment next line
                -- | a == t    = genSt g
                | otherwise = GS 1
        where t = toInteger $ thres g

-- -- | Updates each gene in Chromosome using 'updateLoc'
-- -- Initial argument is an accumulator that counts the effects of binding sites.
-- updateChrom :: Weight -> GeneStateTable -> Chromosome -> Chromosome
-- updateChrom a gst (l:ls) = l' : updateChrom a' gst ls
--     where (a', l') = updateLoc a gst l
-- updateChrom _ _ _ = []
--
-- -- | if Tfbs, increment accumulater with effect
-- -- if Gene, change the expression according to accumulater and set accumulater to 0
-- -- returns a (accumulator, changed Locus) pair
-- updateLoc :: Weight -> GeneStateTable -> Locus -> (Weight, Locus)
-- updateLoc a gst loc@(CTfbs (Tfbs i w))
--         | Map.lookup i gst == Just 1 = (a + w, loc)
--         | otherwise                  = (a, loc)
-- updateLoc a _ (CGene (Gene i t st)) =
--     (0, CGene (Gene i t newState)) where
--         newState    | fromIntegral a <= t = GS 0 --Gene never stays the same: fix change <= to < uncomment next line
--                     -- | fromIntegral a == t = st
--                     | otherwise           = GS 1
-- updateLoc a _ loc = (a, loc)



-- | Groups genes and preceding transcription factors together
-- e.g. [TF, TF, Gene, TF, Gene, TF] -> [[TF, TF, Gene], [TF, Gene], [TF]]
groupGeneTfbs :: [Locus] -> [[Locus]]
groupGeneTfbs [] = []
groupGeneTfbs loci = h: groupGeneTfbs t
    where (h,t) = takeWhileInclusive isGene loci

-- | takes a predicate and returns a pair of lists with the first ending with
-- the element that satisfies the predicate
takeWhileInclusive :: (a -> Bool) -> [a] -> ([a],[a])
takeWhileInclusive p ls = takeWhileInclusive' ([], ls)
    where
    takeWhileInclusive' (a,[]) = (a,[])
    takeWhileInclusive' (a,x:xs) =
        if p x
        then (henk, xs)
        else takeWhileInclusive' (henk,xs)
            where henk = a ++ [x]
                --
                -- > foldr f z []     = z
                -- > foldr f z (x:xs) = x `f` foldr f z xs

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
    return $ map CTfbs $ shuffle' (zipWith3 Tfbs [0..n-1] shuffled (repeat 0)) n' r
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
