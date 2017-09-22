{-# LANGUAGE FlexibleInstances #-}
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

-- import qualified Data.Set as S
type SampleSize = Int
type AnalyzeChrom a = Reader Chromosome a
analyzeChrom :: AnalyzeChrom a -> Chromosome -> a
analyzeChrom = runReader

-- -- | Warning: Slow for large genomes (>40 genes)
-- attrNum :: AnalyzeChrom Int
-- attrNum = length --withStrategy (parList rdeepseq) .
--         . filter id
--         . map (uncurry (==)) --bimap (fmap toOnOff) (fmap toOnOff)) .
--         <$> chromToVertices

-- numRemaining :: Chromosome -> Int
-- numRemaining = length . rmdups . pure . last . map snd . chromToVertices

-- | Return for all # updates (0-11) a count of remaining unique states
numRemaining :: SampleSize -> AnalyzeChrom [Int]
numRemaining samplesize = do
    f <- nsf
    fst . splitAt 12
        . map length
        . iterate (rmdups . map f)
        <$> randomGSTs 42 samplesize

remaining :: SampleSize -> AnalyzeChrom [GST]
remaining samplesize = do
    f <- nsf
    (!!12) . iterate (rmdups . map f)
        <$> randomGSTs 420 samplesize

-- numRemaining :: SampleSize ::

listAttr :: SampleSize -> AnalyzeChrom [Int]
listAttr samplesize = do
    f <- nsf
    full <- fullGST
    -- let g = gstToNum full
    map (gstToNum full . snd) . filter (uncurry (==)) . valueResultPairs f <$> remaining samplesize

attrNum :: SampleSize -> AnalyzeChrom Int
attrNum = (length <$>) . listAttr

targets :: AnalyzeChrom [Int]
targets = do
    full <- fullGST
    return $ map (gstToNum full) [targetGST 0, targetGST 1]


numberInCombination :: GST -> GST -> Int
numberInCombination = undefined

-- | return a list of randomly sampled GeneStateTables
randomGSTs :: Seed -> SampleSize -> AnalyzeChrom [GST]
randomGSTs s samplesize = do
    leFull <- geneCounts
    totalStates <- nrOfStates
    let randoms = withSeed s $ randomsInRange (0,totalStates - 1) samplesize
    return $ startingGST : map (toGST
                  . map (GS . subtract 1)
                  . flip pickFromCombination leFull) randoms

geneCounts :: AnalyzeChrom [Int]
geneCounts = map ((+1) . getGeneState) . toGSL <$> fullGST
-- numRemaining' :: Int -> Chromosome -> Int
-- numRemaining' samplesize c =
--     let f = nsf c
--         full' = fullGST c
--         full = map getGeneState $ toGSL full'
--         randoms = take samplesize $ withSeed (420::Int)
--                     $ randomsInRange (0, product full - 1) samplesize
--      in length . rmdups $ map (f . flip numToGST full' ) randoms


-- | return pairs of GSTs of which the second is the update of the first
chromToVertices :: AnalyzeChrom [(GST,GST)]
chromToVertices = liftM2 valueResultPairs nsf allGST

nsf :: AnalyzeChrom (GST -> GST)
nsf = reader $ \c -> toGST . flip updateChrom c

startGSTAttr :: AnalyzeChrom Int
startGSTAttr = do
    f <- nsf
    full <- fullGST
    return $ gstToNum full $ repeatApply 20 f startingGST

--
-- developpedGST :: GST -> Agent -> GST
-- developpedGST gst ag = geneStateTable $ devAg' $ setAgent gst ag
--
-- setAgent :: GST -> Agent -> Agent
-- setAgent gst ag = ag { geneStateTable = gst }

turnLocusOn :: Locus -> Locus
turnLocusOn (CTfbs t) = CTfbs $ t {tfbsSt = GS 1}
turnLocusOn (CGene g) = CGene $ g {genSt  = GS 1}

turnGeneOn :: Gene -> Gene
turnGeneOn g = g {genSt  = GS 1}

fullGST :: AnalyzeChrom GST
fullGST = reader $ toGST . map (onGene turnGeneOn)


allGST :: AnalyzeChrom [GST]
allGST = allGST' <$!> fullGST
    where allGST' :: InferGST gst => gst -> [GST]
          allGST' = map toGST . allCombinations . toGSL

nrOfStates :: AnalyzeChrom Int
nrOfStates = product <$> geneCounts

prependAll :: [a] -> [[a]] -> [[a]]
prependAll [] _ = []
prependAll (x:xs) ls = map (x:) ls ++ prependAll xs ls
-- prependAll = fix ((`ap` tail) . (. head) . flip ((.) . ap . ((++) .) . map . (:)))

allCombinations :: Integral a => [a] -> [[a]]
allCombinations = foldr (\ n -> prependAll [0 .. n]) [[]]

toOnOff :: GeneState -> GeneState
toOnOff (GS 0) = GS 0
toOnOff _      = GS 1

-- sampledGST :: Chromosome -> Int -> [GST']
-- sampledGST c = undefined
--     where full = fullGST c

gstToNum :: GST -> GST -> Int
gstToNum full' this' = antiPickFromCombination this full
    where this = map (getGeneState . toOnOff) $ toGSL this'
          full = map getGeneState $ toGSL full'

numToGST :: Int -> GST -> GST
numToGST i gst = toGST $ uncurry pickFromCombination $ bimap id toIntList (i, gst)
    where toIntList = map getGeneState . M.elems

agent42 :: Agent
agent42 = read "Agent {genome = [[CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 2, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 8, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 0, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 7, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 16, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 5, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 10, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 13, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 14, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 18, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 9, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 17, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 19, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 3, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 11, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 12, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 15, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 1, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 4, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 6, thres = Thres 0, genSt = GS 0})]], geneStateTable = fromList [(ID 0,GS 0),(ID 1,GS 0),(ID 2,GS 0),(ID 3,GS 0),(ID 4,GS 0),(ID 5,GS 1),(ID 6,GS 0),(ID 7,GS 0),(ID 8,GS 1),(ID 9,GS 1),(ID 10,GS 0),(ID 11,GS 1),(ID 12,GS 0),(ID 13,GS 0),(ID 14,GS 1),(ID 15,GS 0),(ID 16,GS 0),(ID 17,GS 0),(ID 18,GS 1),(ID 19,GS 0)], bornTime = 0, bornEnv = 0, parent = NoAgent, diff = []}"


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
