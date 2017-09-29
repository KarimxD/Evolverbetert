{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Landscapes
    where

import Types
-- import qualified Parameters as P
import Misc -- (valueResultPairs, rmdups, horizontalHistogram, pickFromCombination, repeatApply)--, antiPickFromCombination)
import World --(updateChrom)
import Data.Bifunctor
import qualified Data.Map.Strict as M
-- import Data.Graph.Inductive
import MyRandom (withSeed, randomsInRange)
import Control.Monad.Reader
import Fitness (targetGST, startingGST)

import Data.List (sort)

-- import qualified Data.Set as S
type SampleSize = Int
type AnalyzeChrom a = Reader Chromosome a

analyzeChrom :: AnalyzeChrom a -> Chromosome -> a
analyzeChrom = runReader

rmdupsWithCount :: [(GST, Int)] -> [(GST, Int)]
rmdupsWithCount = sort . rmdupsWith addgst (\a b -> fst a == fst b)

addgst :: (GST, Int) -> (GST, Int) -> (GST, Int)
addgst (g, !i) (_, !j) = (g, i+j)

toCountList :: Ord a => [a] -> [(a, Int)]
toCountList = map (\x -> (x,1)) . sort

rmdupsWith :: Ord a => (a -> a -> a) -> (a -> a -> Bool) -> [a] -> [a]
rmdupsWith f' eq' = rmdupsWith' f' eq' . sort
    where
        rmdupsWith' _ _ []           = []
        rmdupsWith' _ _ [x]          = [x]
        rmdupsWith' f eq (x1:x2:xs)
            | x1 `eq` x2 =      rmdupsWith' f eq $ f x1 x2 : xs
            | otherwise  = x1 : rmdupsWith' f eq       (x2 : xs)


-- | Return for all # updates (0-11) a count of remaining unique states
numRemaining :: SampleSize -> AnalyzeChrom [Int]
numRemaining samplesize = do
    f <- nsf
    take 12
        . map length
        . iterate (rmdups . map f)
        <$> randomGSTs 42 samplesize

-- | A list of remaining unique states after updating n times
remaining :: Int -- ^ The number of updates
          -> SampleSize -> AnalyzeChrom [(GST, Int)]
remaining n samplesize = do
    f <- nsfWithCount
    (!!n) . iterate (rmdupsWithCount . map f) . toCountList
        <$> randomGSTs 420 samplesize

nsfWithCount :: AnalyzeChrom ((GST, Int) -> (GST, Int))
nsfWithCount = first <$> nsf

-- | A list of attractors and their basin of attraction
listAttr :: SampleSize -> AnalyzeChrom [(Int, Int)]
listAttr samplesize = do
    convert <- gstToNum
    map (first convert) <$> remaining 100 samplesize

-- | A count of the number of attractors with a certain samplesize
attrNum :: SampleSize -> AnalyzeChrom Int
attrNum ss = length <$> listAttr ss

-- | The targetStates of the model
targets :: AnalyzeChrom [Int]
targets = mapM (ap gstToNum . return) [targetGST 0, targetGST 1]

-- | Return a list of randomly sampled GeneStateTables
randomGSTs :: Seed -> SampleSize -> AnalyzeChrom [GST]
randomGSTs s samplesize = do
    leFull <- geneCounts
    totalStates <- nrOfStates
    let randoms = withSeed s $ randomsInRange (0,totalStates - 1) samplesize
    return $ map (toGST
                  . map (GS . subtract 1)
                  . flip pickFromCombination leFull) randoms

-- | A count of all genes in the genome in ascending order of geneID
geneCounts :: AnalyzeChrom [Int]
geneCounts = map ((+1) . getGeneState) . toGSL <$> fullGST

-- | Return pairs of GSTs of which the second is the update of the first
chromToVertices :: AnalyzeChrom [(GST,GST)]
chromToVertices = liftM2 valueResultPairs nsf allGST

-- | The updating function for GSTs
nsf :: AnalyzeChrom (GST -> GST)
-- nsf = reader $ \c -> toGST . flip updateChrom c
nsf = do
    c <- ask
    return $ \gst -> toGST $ updateChrom gst (setChrom gst c)
        where setChrom gst = map (onGene $ \g -> g {genSt = gst M.! geneID g})

-- | The attractor the startingGST goes to
startGSTAttr :: AnalyzeChrom Int
startGSTAttr = do
    f <- nsf
    gstToNum <*> pure (repeatApply 20 f startingGST)

-- | A GST with all Genes maximum on
fullGST :: AnalyzeChrom GST
fullGST = reader $ toGST . map (onGene turnGeneOn)

-- | Returns all states a Chromosome can be in (Warning: Slow!)
allGST :: AnalyzeChrom [GST]
allGST = allGST' <$!> fullGST
    where allGST' :: InferGST gst => gst -> [GST]
          allGST' = map toGST . allCombinations . toGSL

-- | Returns the number of states a Chromosome can be in
nrOfStates :: AnalyzeChrom Int
nrOfStates = product <$> geneCounts

toDot :: AnalyzeChrom String
toDot = do
    leaves <- remaining 4 1000
    f <- nsfWithCount
    let edges = valueResultPairs f leaves
        newleaves = rmdupsWithCount $ map snd edges
        edges2 = valueResultPairs f newleaves
    return ""

makeEdges'' :: AnalyzeChrom [(Int,Int)]
makeEdges'' = do
    list <- makeEdges
    g    <- gstToNum
    let list' = map f list
    return $ map (bimap g g) list'

    where f ((a,_), (b,_)) = (a, b)

makeEdges :: AnalyzeChrom [((GST,Int),(GST,Int))]
makeEdges = do
    leaves <- remaining 0 100
    let eq ((g,_),(f,_)) ((h,_),(j,_)) = (g,f) == (h,j)

    rmdupsWith biggest eq <$> makeEdges' 12 leaves
    where
          biggest f@((_,i1),_) g@((_,j1),_)
            | i1 > j1 = f
            | otherwise = g
        --   initials =

makeEdges' :: Int -> [(GST,Int)] -> AnalyzeChrom [((GST,Int),(GST,Int))]
makeEdges' n leaves
    | n == 0    = return []
    | otherwise = do
        f <- nsfWithCount
        let edges = valueResultPairs f leaves
            newleaves = rmdupsWithCount $ map snd edges
        (edges ++) <$> makeEdges' (n - 1) newleaves

stateNetwork :: AnalyzeChrom String
stateNetwork = do
    edges <- makeEdges''
    let edgelines = map f edges
    return $ "SOURCE\tTarget\n" ++
           unlines edgelines
        where
              f (i,j) = show i ++ "\t" ++ show j

allStateNetwork :: AnalyzeChrom String
allStateNetwork = do
    edges <- makeEdges''
    let edgelines = map f edges
    return $ "SOURCE\tTarget\n" ++
           unlines edgelines
        where
            f (i,j) = show i ++ "\t" ++ show j

            makeEdges'' :: AnalyzeChrom [(Int,Int)]
            makeEdges'' = do
                list <- makeEdges
                g    <- gstToNum
                let list' = map f list
                return $ map (bimap g g) list'

                where f ((a,_), (b,_)) = (a, b)

            makeEdges :: AnalyzeChrom [((GST,Int),(GST,Int))]
            makeEdges = do
                leaves <- map (\g -> (g,1)) <$> allGST
                let eq ((g,_),(f,_)) ((h,_),(j,_)) = (g,f) == (h,j)

                rmdupsWith biggest eq <$> makeEdges' 12 leaves
                where
                      biggest f@((_,i1),_) g@((_,j1),_)
                        | i1 > j1 = f
                        | otherwise = g


















-- | Prepend all lists with all combinations of elements from the first list
prependAll :: [a] -> [[a]] -> [[a]]
prependAll [] _ = []
prependAll (x:xs) ls = map (x:) ls ++ prependAll xs ls

-- | Best explained by example:
--
-- prop> allCombingetLastChromations [1,1,3] = [[0,0,0],[0,0,1],[0,0,2],[0,0,3],[0,1,0],[0,1,1],[0,1,2],[0,1,3],[1,0,0],[1,0,1],[1,0,2],[1,0,3],[1,1,0],[1,1,1],[1,1,2],[1,1,3]]
--
allCombinations :: Integral a => [a] -> [[a]]
allCombinations = foldr (\n -> prependAll [0 .. n]) [[]]

-- | Not reversible with numToGST. Reduces all states to either 0 or 1
gstToNum :: AnalyzeChrom (GST -> Int)
gstToNum = do
    full <- toOnOffList <$> fullGST
    return $ \this -> antiPickFromCombination (toOnOffList this) full
        where toOnOffList = map (getGeneState . toOnOff) . toGSL

-- | Converts a number to a GST
numToGST :: AnalyzeChrom (Int -> GST)
numToGST = do
    gst <- fullGST
    return $ \i -> toGST $ uncurry pickFromCombination $ bimap id toIntList (i, gst)
        where toIntList = map getGeneState . M.elems

-- | prop> toOnOff (GS 0) = GS 0
--   prop> toOnOff _      = GS 1
toOnOff :: GeneState -> GeneState
toOnOff (GS 0) = GS 0
toOnOff _      = GS 1

-- | Also turns a gene with state > 1 to state = 1
turnLocusOn :: Locus -> Locus
turnLocusOn (CTfbs t) = CTfbs $ t {tfbsSt = GS 1}
turnLocusOn (CGene g) = CGene $ g {genSt  = GS 1}

-- | Set genSt of Gene to 1
turnGeneOn :: Gene -> Gene
turnGeneOn g = g {genSt  = GS 1}







agent42 :: Agent
agent42 = read $ "Agent {genome = [[CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 2, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 8, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 0, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 7, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 16, thres = Thres 2, genSt = GS 0}),"
    ++ "CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 5, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 10, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 13, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 14, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 18, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 9, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 17, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 19, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 3, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 11, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 12, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 15, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 1, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 4, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 6, thres = Thres 0, genSt = GS 0})]], geneStateTable = fromList [(ID 0,GS 0),(ID 1,GS 0),(ID 2,GS 0),(ID 3,GS 0),(ID 4,GS 0),(ID 5,GS 1),(ID 6,GS 0),(ID 7,GS 0),(ID 8,GS 1),(ID 9,GS 1),(ID 10,GS 0),(ID 11,GS 1),(ID 12,GS 0),(ID 13,GS 0),(ID 14,GS 1),(ID 15,GS 0),(ID 16,GS 0),(ID 17,GS 0),(ID 18,GS 1),(ID 19,GS 0)], bornTime = 0, bornEnv = 0, parent = NoAgent, diff = []}"

-- data Node a = Node { identity :: a
--                    , numIn      :: Int
--                    , inNodes    :: [Node a]
--                    , allInNodes :: [a]
--                    }
-- instance GST (Node GST) where
--     toGST = identity
--
-- type Vertice = (GST,GST)
--
--
-- nsfNode :: Chromosome -> Node GST -> Maybe (Node GST)
-- nsfNode c n =
--     if nextGST `elem` allInNodes n
--     then Nothing
--     else Just Node { identity   = nextGST
--                    , numIn      = 1
--                    , inNodes    = [n]
--                    , allInNodes = [source]
--                    }
--     where
--         source = identity n
--         nextGST = nsf c source

-- attrNum' :: Chromosome -> Int
-- attrNum' = length . filter id . map (uncurry (==)) . edges . chromToGraph

-- chromToGraph :: AnalyzeChrom (Gr GST ())
-- chromToGraph = do
--     c <- ask
--     allNodes <- zip [0..] <$> allGST
--     let nodeMap = M.fromList $ map swap allNodes
--     return $ mkGraph allNodes (blub allEdges)
--     where
--           allEdges = zip [0..] $
--                      map (getLeNode . nsf c . snd) allNodes
--           getLeNode n = case M.lookup n nodeMap
--             of Just x -> x
--                _      -> error $ myShow n ++ "\n" ++
--                             horizontalHistogram (map iD $ toGenes c)
--           blub = map (\(a,b) -> (a,b,()))

-- -- | Warning: Slow for large genomes (>40 genes)
-- attrNum :: AnalyzeChrom Int
-- attrNum = length --withStrategy (parList rdeepseq) .
--         . filter id
--         . map (uncurry (==)) --bimap (fmap toOnOff) (fmap toOnOff)) .
--         <$> chromToVertices

-- numRemaining :: Chromosome -> Int
-- numRemaining = length . rmdups . pure . last . map snd . chromToVertices
