{-# LANGUAGE BangPatterns #-}

module World
-- (
    -- World, Env, Agents, Agent(..), Genome, Chromosome, GeneStateTable, Locus(..), Gene(..), Tfbs(..), ID, Thres, GeneState
    -- , devAg, agent0, groupGeneTfbs, gSTFromGenome, fitnessAgent, showGST, hammDist, targetGST, hammDistAg)
    where
import Control.Monad
import qualified Data.List         as List
import qualified Data.Map          as Map
import           Data.Array.IArray
import           Misc
import qualified Parameters        as P
import           Data.List.Split     (splitOn)
import MyRandom
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import Control.Monad.State (state)



type Env = Int
-- type World = (Agents,Env)
data World = World {    agents :: Agents
                    ,   env :: Env}

type Agents = Array (Int, Int) Agent
data Agent = Agent {    genome         :: Genome
                    ,   geneStateTable :: GeneStateTable}
           | NoAgent deriving (Show, Read, Eq, Ord)
type Genome = [Chromosome]
type Chromosome = [Locus]
type GeneStateTable = Map.Map ID GeneState
showGST = List.intersperse ' ' . concatMap (show . snd) . Map.toList :: GeneStateTable -> String
data Locus
    = Transposon
    | CGene     Gene
    | CTfbs     Tfbs     deriving (Eq, Ord, Read)
instance Show Locus where
    show Transposon = "T"
    show (CGene (Gene i t gs)) = "G" ++ show i ++ ":" ++ show t ++ ":" ++ show gs
    show (CTfbs (Tfbs i w)) = show i ++ ":" ++ show w
parseLoc str
    | h == 'G'   = CGene $ Gene (read $ tail $ head s) (read $ s!!1) 0 --(read $ s!!2)
    | str == "T" = Transposon
    | otherwise  = CTfbs $ Tfbs (read $ head s) (read $ s!!1)
        where h = head str; s = splitOn ":" str



data Gene = Gene {      iD :: ID
                    ,   thres :: Thres
                    ,   genSt :: GeneState } deriving (Show, Read, Eq)
instance Ord Gene where
    Gene i1 t1 gs1 `compare` Gene i2 t2 gs2 =
        if i1 == i2
        then if gs1 == gs2
             then compare t1 t2   --small thres first
             else compare gs2 gs1 --largest state firster
        else compare i1 i2        --smaller id firstest


data Tfbs = Tfbs ID Weight deriving (Show, Read, Eq, Ord)

type ID = Int
type Thres = Int
type GeneState = Int --deriving (Show, Read, Eq, Ord)
type Weight = Int

devAg :: Agent -> Agent
devAg = takeUntilSame . take P.devTime . iterate updateAgent
    where
    takeUntilSame [a,b] = NoAgent
    takeUntilSame (a:b:rest) =
        if sameGST a b
         then   a
         else   takeUntilSame $ b:rest

sameGST :: Agent -> Agent -> Bool
sameGST NoAgent _ = True
sameGST _ NoAgent = False
sameGST ag1 ag2 = geneStateTable ag1 == geneStateTable ag2


defaultGst = Map.fromList defaultStates
    where defaultStates = take P.nrGeneTypes $ zip [0..] (repeat 0) :: [(ID,GeneState)]

-- | Check if locus is effected by expressed transcription factors
-- | Return the effect it has. 0 is no effect.
locusEffect :: Locus -> GeneStateTable -> Int
locusEffect (CTfbs (Tfbs i w)) gst
    | Map.lookup i gst == Just 1 = w
    | otherwise                  = 0
locusEffect _ _ = 0

updateAgent :: Agent -> Agent
updateAgent NoAgent = NoAgent
updateAgent (Agent !chroms !gst) =
    if length newGST == P.nrGeneTypes
        then Agent newGenome newGST
        else NoAgent
    where newGenome = updateGenome gst chroms
          newGST = gSTFromGenome newGenome

updateGenome :: GeneStateTable -> Genome -> Genome
updateGenome = map . updateChrom 0

updateChrom :: Int -> GeneStateTable -> Chromosome -> Chromosome
updateChrom accum gst (l:ls) = l' : updateChrom accum' gst ls
    where (accum', l') = updateLoc accum gst l
updateChrom _ _ _ = []

updateLoc :: Int -> GeneStateTable -> Locus -> (Int, Locus)
updateLoc accum gst tfbs@(CTfbs (Tfbs i w))
        | Map.lookup i gst == Just 1 = (accum + w, tfbs)
        | otherwise                  = (0, tfbs)
updateLoc accum _ loc@(CGene gen@(Gene i t st)) =
    (0, CGene (Gene i t newState)) where
        newState    | accum < t  = 0
                    | accum == t = st
                    | otherwise = 1
updateLoc accum _ l = (accum, l)

isGene :: Locus -> Bool
isGene x = case x of (CGene _) -> True; _ -> False

-- | e.g. [TF, TF, Gene, TF, Gene, TF] -> [[TF, TF, Gene], [TF, Gene], [TF]]
groupGeneTfbs :: [Locus] -> [[Locus]]
groupGeneTfbs [] = []
groupGeneTfbs ls = h: groupGeneTfbs t
    where f = takeWhileInclusive isGene
          (h,t) = f ([],ls)

takeWhileInclusive :: (a -> Bool) -> ([a],[a]) -> ([a],[a])
takeWhileInclusive _ (accum,[]) = (accum,[])
takeWhileInclusive f (accum,x:xs) =
    if f x
        then (henk, xs)
        else takeWhileInclusive f (henk,xs)
            where henk = accum ++ [x]

-- | Generate GST from a genome
gSTFromGenome :: Genome -> GeneStateTable
gSTFromGenome genes = makeGst Map.empty $ reduceToGenes genes


makeGst :: GeneStateTable -> [Gene] -> GeneStateTable
makeGst gst [] = gst
makeGst gst (g:genes) = makeGst (Map.insertWith max k val gst) genes
    where k = iD g
          val = genSt g

-- | Concatinate chromosomes and trim them of non-coding genes
--   fix for genomes with more genes
reduceToTfbss :: Genome -> [Tfbs]
reduceToTfbss = filterTfbss . head
    where
        filterTfbss []             = []
        filterTfbss (CTfbs g:rest) = g : filterTfbss rest
        filterTfbss (_:rest)       = filterTfbss rest

reduceToGenes :: Genome -> [Gene]
reduceToGenes = filterGenes . head
    where
        filterGenes []             = []
        filterGenes (CGene g:rest) = g : filterGenes rest
        filterGenes (_:rest)       = filterGenes rest

fitnessAgent :: Agent -> Env -> Double
fitnessAgent (Agent _ gst) e = fitnessGST e gst
fitnessAgent NoAgent _      = 0

-- | Uses targetGST to check fitness of passed GST
fitnessGST :: Env -> GeneStateTable -> Double
fitnessGST !env !gst = (1 - d / dmax)^p
    where
        p = P.selectionPressure
        dmax = fromIntegral P.nrGeneTypes
        d = fromIntegral $ hammDist target this
            where target = Map.toList (targetGST env)
                  this = Map.toList gst

hammDist :: (Eq a) => [a] -> [a] -> Int
-- hammDist = ((length . filter (True ==)) .) . zipWith (/=)
hammDist [] _ = 0
hammDist _ [] = 0
hammDist (a:as) (b:bs) = if a /= b then 1 + hammDist as bs else hammDist as bs
{-# SPECIALIZE hammDist :: [(Int,Int)] -> [(Int,Int)] -> Int #-}

hammDistAg :: Agent -> Env -> Int
hammDistAg NoAgent _ = fromIntegral P.nrGeneTypes
hammDistAg ag env = hammDist (Map.toList (targetGST env)) (Map.toList $ geneStateTable ag)

-- | Generate GeneStateTable based on targetExpression
targetGST :: Env -> GeneStateTable
targetGST 0 = Map.fromList $ valueResultPairs (targetExpression 0) [0..P.nrGeneTypes-1]
targetGST 1 = Map.fromList $ valueResultPairs (targetExpression 1) [0..P.nrGeneTypes-1]
targetGST env = Map.fromList $
    take P.nrGeneTypes $ valueResultPairs (targetExpression env) [0..]


targetExpression :: Env -> ID -> Int
{- |
example for nrEnv = 4 and nrHousInteractionType0   1   0   1   0   0   0
2           1   1   1   1   1   1   0   0   0   1   0   0
3           1   1   1   1   0   1   1   0   0   0   1   0
-}
targetExpression env i
    | i <  hh                                   = 1 -- household

    | i < hh + ov &&
        (i - hh - env) `mod` P.nrEnv == 0       = 0
    | i < hh + ov &&
        (i - hh - env) `mod` P.nrEnv /= 0       = 1 -- overlapping

    | (i - hh - ov - env) `mod` P.nrEnv == 0    = 1
    | otherwise                                 = 0 -- specific
    where hh = P.nrHouseHold; ov = P.nrOverlap; sp = P.nrSpecific














agent0 :: Agent
agent0 = devAg $ Agent genome0 defaultGst
    where
            genome0 = [map parseLoc $ splitOn "," "18:6,15:-2,G1:-1:0,0:-4,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-2,15:3,G15:-1:0,G7:-3:0,T,T,7:4,6:2,G14:-1:0,15:-6,10:5,G19:-1:0,10:-2,3:-6,17:2,G11:-2:0,G6:-1:0,T,18:-5,3:-1,G9:-3:0,G3:-3:0,13:-5,8:4,G2:-1:0,11:-5,7:4,G16:-1:0,T,13:3,6:5,1:-6,G18:-1:0,G17:-1:0,6:0,2:5,G0:-3:0,17:4,6:-1,G8:-1:0,8:5,16:-4,G4:-3:0,G13:-1:1"]

-- genome0 = [map parseLoc $ splitOn "," "18:1,15:-2,G1:-1:0,0:-1,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-1,15:1,G15:-1:0,G7:-2:0,T,T,7:1,6:1,G14:-1:0,15:-1,10:1,G19:-1:0,10:-1,3:-1,17:1,G11:-2:0,G6:-1:0,T,18:-1,3:-1,G9:-2:0,G3:-2:0,13:-1,8:1,G2:-1:0,11:-1,7:1,G16:-1:0,T,13:1,6:1,1:-1,G18:-1:0,G17:-1:0,6:1,2:1,G0:-2:0,17:1,6:-1,G8:-1:0,8:1,16:-1,G4:-2:0,G13:-1:1"]
-- genome0 = [map parseLoc $ splitOn "," "14:-1,9:-1,G17:0,16:-1,G12:0,G19:-1,6:-1,4:1,G16:0,G18:0,17:-1,9:-1,8:-1,G0:0,G14:0,9:-1,4:-1,G10:0,9:-1,2:-1,G6:-1,3:-1,G5:-1,7:-1,G1:-1,19:-1,G7:-1,17:1,4:-1,G2:-1,G9:-1,G11:-1,G4:1,17:-1,11:-1,17:1,G8:1,7:1,G3:1,10:-1,G13:1,10:-1,3:-1,G15:1"]



randomAgent :: Rand Agent
randomAgent = do
    genome <- goodRandomGenome
    let  agent = devAg $ Agent genome defaultGst
    if   agent == NoAgent
        then randomAgent
        else return agent

connected :: Genome -> Bool
connected = all (>1) . map length . groupGeneTfbs . concat

goodRandomGenome :: Rand Genome
goodRandomGenome = do
    genome <- randomGenome
    if      connected genome
       then return genome
       else goodRandomGenome

randomGenome :: Rand Genome
randomGenome = fmap (:[]) randomChromosome

randomChromosome :: Rand Chromosome
randomChromosome = do
    r <- getModifyRand
    randomChrom <- concat <$> sequence [randomTfbs,randomTfbs,randomGene]
    return $ shuffle' randomChrom (length randomChrom) r

randomTfbs :: Rand [Locus]
randomTfbs = do
    randomWeights <- replicateM n $ state randomW

    r <- getModifyRand
    let shuffled = shuffle' randomWeights n r
    return $ map CTfbs $ shuffle' (zipWith Tfbs [0..n-1] shuffled) n r
        where
            n = P.nrGeneTypes
            randomW :: PureMT -> (Int, PureMT)
            randomW g = let (d, g') = randomBool g
                        in if d then (-1, g') else (1, g')

randomGene :: Rand [Locus]
randomGene = do
    randomThresholds <- replicateM n $ getRange (P.minThres, P.maxThres)
    r <- getModifyRand
    let shuffled = shuffle' randomThresholds n r
    return $ map CGene $ shuffle' (zipWith makeGene [0..n-1] shuffled) n r
        where n = P.nrGeneTypes
              makeGene i t = Gene i t 0
