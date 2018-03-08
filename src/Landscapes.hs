{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE BangPatterns #-}

module Landscapes where


import Types
-- import qualified Parameters as P
import Misc -- (valueResultPairs, rmdups, horizontalHistogram, pickFromCombination, repeatApply)--, antiPickFromCombination)
import World --(updateChrom)
import Parameters
import MyRandom (withSeed, randomsInRange)
import Fitness

import qualified Data.Map.Strict as M
-- import Data.Graph.Inductive
import Control.Monad.Reader
import Data.List
import Data.Function (on)
import Data.Maybe
import Data.Ord
import TextShow.TH
import qualified Data.Text as T
import Safe

{-# ANN module ("HLint: ignore Use &&&" :: String) #-}


-- import qualified Data.Set as S
type SampleSize = Int
type AnalyzeChrom a = Reader Chromosome a
data Parameters = Parameters { sampleSize :: SampleSize
                             , numberOfUpdates :: Int
                             , seed :: Seed
                             , chromConverter :: Chromosome -> T.Text
                             , resetGeneStatesOnBirth :: Bool
                             }
type Attractor = (Int   -- | Attractor
                , Int   -- | Basin of attraction
                , [Int] -- | List of HammDists to targets
                )--
--
-- data Attractor' =
--       CyclicAttractor { states :: [GST]
--                       , basin  :: Int
--                       }
--     | PointAttractor { states :: [GST]
--                      , basin :: Int
--                      }

isPointAttr :: AnalyzeChrom (Attractor -> Bool)
isPointAttr = do
    f <- nsf'
    return $ \(a, _, _) -> f a == a

data Node = Node { nodeGST :: GST
                 , nodeBasin :: Int
                 , nodeProperty :: [String]
                 } deriving (Show, Read)
instance Eq Node where
    n1 == n2 = ((==) `on` nodeGST)      n1 n2
            -- && ((==) `on` nodeProperty) n1 n2
instance Ord Node where
    compare = comparing nodeGST
instance InferGST Node where
    toGST = nodeGST
instance HammDist Node
instance HasFitness Node



startingNode :: Node
startingNode = Node startingGST 1 ["start"]

type Edge = (Node, Node)

analyzeChrom :: AnalyzeChrom a -> Chromosome -> a
analyzeChrom = runReader

rmdupsWithCount :: [Node] -> [Node]
rmdupsWithCount = sort . rmdupsWith compare addNodes (==)

addNodes :: Node -> Node -> Node
addNodes n m = n { nodeBasin = ((+)  `on` nodeBasin) n m
                 , nodeProperty = rmdups $ ((++) `on` nodeProperty) n m
                 }

toCountList :: Ord a => [a] -> [(a, Int)]
toCountList = map (\x -> (x,1)) . sort

toNodes :: [GST] -> [Node]
toNodes = map toNode

toNode :: GST -> Node
toNode g = Node {nodeGST = g, nodeBasin = 1, nodeProperty = []}

rmdupsWith :: (a -> a -> Ordering) -> (a -> a -> a) -> (a -> a -> Bool) -> [a] -> [a]
rmdupsWith g f eq = go . sortBy g
    where
        go (x1:x2:xs)
            | x1 `eq` x2 =      go $ f x1 x2 : xs
            | otherwise  = x1 : go       (x2 : xs)
        go x = x


-- | Return for all # updates (0-11) a count of remaining unique states
numRemaining :: Parameters -> AnalyzeChrom [Int]
numRemaining params = do
    f <- nsf
    take 12
        . map length
        . iterate (rmdups . map f)
        <$> randomGSTs params

-- | A list of remaining unique states after updating n times
remaining :: Parameters -> AnalyzeChrom [Node]
remaining params@Parameters {numberOfUpdates = n} = do
    f <- nsfWithCount
    (!!n) . iterate (rmdupsWithCount . map f) . toNodes
        <$> randomGSTs params

nsfWithCount :: AnalyzeChrom (Node -> Node)
nsfWithCount = do
    f <- nsf
    return $ \n -> n {nodeGST = f (nodeGST n)}


-- | A list of attractors and their basin of attraction
--   returns (Attr, )
listAttr :: Parameters
         -> AnalyzeChrom [(Int   -- | Attractor
                         , Int   -- | Basin of attraction
                         , [Int] -- | List of HammDists to targets
                         )]
listAttr params = do
    -- (attractors, basins) <- unzip <$> remaining 100 samplesize
    nodes <- remaining (params {numberOfUpdates = 100})
    let (attractors, basins) = unzip $ map (\n -> (nodeGST n, nodeBasin n)) nodes

    attractors' <- map <$> gstToNum <*> return attractors
    let envs = [0..nrEnv-1]
        fs = map hammDist envs :: [GST -> Int]
        hds = map (\a -> map ($ a) fs) attractors :: [[Int]]
    return $ zip3 attractors' basins hds

-- | A count of the number of attractors with a certain samplesize
attrNum :: Parameters -> AnalyzeChrom Int
attrNum ss = length <$> listAttr ss

listPointAttr :: Parameters -> AnalyzeChrom [Attractor]
listPointAttr ss = do
    list <- listAttr ss
    p <- isPointAttr
    return $ filter p list

-- listCyclicAttr :: Parameters -> AnalyzeChrom [Attractor]
-- listCyclicAttr =

theAttractor :: Parameters -> AnalyzeChrom (Maybe Attractor)
theAttractor params = do
    list <- listAttr params
    attr <- stateOfChrom
    return $ find (\(x,_,_) -> x == attr) list

theAttractorBasin :: Parameters -> AnalyzeChrom (Maybe Int)
theAttractorBasin params = fmap (\(_,x,_) -> x) <$> theAttractor params

-- | A count of the number of point attractors with a certain samplesize
pointAttrNum :: Parameters -> AnalyzeChrom Int
pointAttrNum ss = length <$> listPointAttr ss

-- | The targetStates of the model
targets :: AnalyzeChrom [Int]
targets = mapM (ap gstToNum . return) [targetGST 0, targetGST 1]

-- | Return a list of randomly sampled GeneStateTables
randomGSTs :: Parameters -> AnalyzeChrom [GST]
randomGSTs params = do
    randoms <- randomGSTs' params
    convert <- numToGST
    return $ map convert randoms

randomNodes :: Parameters -> AnalyzeChrom [Node]
randomNodes params = toNodes <$> randomGSTs params

allNodes :: AnalyzeChrom [Node]
allNodes = toNodes <$> allGST

-- | Return a list of randomly sampled Ints (corresponding to GSTs)
randomGSTs' :: Parameters -> AnalyzeChrom [Int]
randomGSTs' Parameters {sampleSize = ss, seed = s} = do
    totalStates <- nrOfStates
    return $ withSeed s $ randomsInRange (0,totalStates - 1) ss

-- | A count of all genes in the genome in ascending order of geneID
geneCounts :: AnalyzeChrom [Int]
geneCounts = map ((+1) . getGeneState) . toGSL <$> fullGST



-- | The updating function for GSTs
nsf :: AnalyzeChrom (GST -> GST)
-- nsf = reader $ \c -> toGST . flip updateChrom c
nsf = do
    c <- ask
    -- return $ \g -> toGST $ updateChrom g c
    return $ \gst -> toGST $ updateChrom gst (setChrom gst c)
        where setChrom gst = map (onGene $ \g -> g {genSt = gst M.! geneID g})

nsf' :: AnalyzeChrom (Int -> Int)
nsf' = do
    f <- nsf
    g <- gstToNum
    n <- numToGST
    return $ g . f . n

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

-- makeEdges :: Int -> [Node] -> AnalyzeChrom [Edge]
-- makeEdges n leaves
--     | n == 0    = return []
--     | otherwise = do
--         f <- nsfWithCount
--         let edges = rmDupEdges False $ valueResultPairs f leaves
--             (_,newleaves) = unzip edges
--         moreEdges <- makeEdges (n - 1) newleaves
--         return $ edges ++ moreEdges

makeEdges :: Int -> [Node] -> AnalyzeChrom [Edge]
makeEdges n accum
    | n == 0    = do
        f <- nsfWithCount
        return $ valueResultPairs f accum
    | otherwise = do
        f <- nsfWithCount
        let out = rmdupsWithCount $ map f accum
        makeEdges (n-1) (accum ++ out)

stateOfChrom :: AnalyzeChrom Int
stateOfChrom = gstToNum <*> (toGST <$> ask)

updateTillSmaller :: [Node] -> Int -> AnalyzeChrom [Node]
updateTillSmaller input n = do
    f <- nsfWithCount
    return $ fromJust $ find ((<=n) . length) $ iterate (rmdupsWithCount . map f) input
    -- | length input <= n
    --   = return input
    -- | otherwise = do
    --     f <- nsfWithCount
    --     let out =  map f $ rmdupsWithCount input
    --     updateTillSmaller out n

        -- remaining :: Int -- ^ The number of updates
        --           -> Parameters -> AnalyzeChrom [Node]
        -- remaining n samplesize = do
        --     f <- nsfWithCount
        --     (!!n) . iterate (map f . rmdupsWithCount) . toNodes
        --         <$> randomGSTs 420 samplesize


stateNetwork :: Parameters -> AnalyzeChrom String
stateNetwork params = do
    f <- nsfWithCount
    nodes <- take 500 . nub --rmdupsWith max' (==)
            . reverse . concat . take 100 . iterate (rmdupsWithCount . map f)
             <$> randomNodes params
    let edges = valueResultPairs f nodes
    printEdges edges
        -- where max' = maxBy (comparing nodeBasin)
        --       maxBy :: (a -> a -> Ordering) -> a -> a -> a
        --       maxBy f x y = case f x y of
        --           LT -> y
        --           _  -> x
            --   unOrdering :: Ordering -> Ordering
            --   unOrdering EQ = EQ
            --   unOrdering GT = LT
            --   unOrdering LT = GT
            --   unCompare :: Ord a => a -> a -> Ordering
            --   unCompare x y = unOrdering $ compare x y

    -- -- | A list of remaining unique states after updating n times
    -- remaining :: Int -- ^ The number of updates
    --           -> Parameters -> AnalyzeChrom [Node]
    -- remaining n samplesize = do
    --     f <- nsfWithCount
    --     (!!n) . iterate (rmdupsWithCount . map f) . toNodes
    --         <$> randomGSTs 420 samplesize

printEdges :: [Edge] -> AnalyzeChrom String
printEdges edges = do
    gtn <- gstToNum
    let f (s, t) =     show (gtn $ nodeGST s)
            ++ "\t" ++ show (gtn $ nodeGST t)
            ++ "\t" ++ show (nodeBasin t)
            ++ "\t" ++ show (nodeProperty s)
            ++ "\t" ++ show (hammDist 0 s)
            ++ "\t" ++ show (hammDist 1 s)
            ++ "\t" ++ show (snd $ best s)
            ++ "\t" ++ show (fst $ best s)
        edgelines = map f edges
    return $ "SOURCE\tTARGET\tBASIN\tPROPERTY\tHD0\tHD1\tSMALLEST_HD\tBEST_ENV\n" ++
           unlines edgelines
        where best x
                | hammDist 0 x <= hammDist 1 x = (0, hammDist 0 x) :: (Int, Int)
                | otherwise                    = (1, hammDist 1 x)



allStateNetwork :: AnalyzeChrom String
allStateNetwork = do
    f <- nsfWithCount
    nodes <- nub --rmdupsWith max' (==)
        . reverse . concat . take 100 . iterate (rmdupsWithCount . map f)
         <$> allNodes
    let edges = valueResultPairs f nodes
    printEdges edges
  -- do
  --   edges <- makeEdges''
  --   let edgelines = map f edges
  --   return $ "SOURCE\tTARGET\n" ++
  --          unlines edgelines
  --       where
  --           f (i,j) = show i ++ "\t" ++ show j
  --
  --           makeEdges'' :: AnalyzeChrom [(Int,Int)]
  --           makeEdges'' = do
  --               list <- makeEdges'
  --               g    <- gstToNum
  --               let list' = map f list
  --               return $ map (bimap g g) list'
  --
  --               where f ((a,_), (b,_)) = (a, b)
  --
  --           makeEdges' :: AnalyzeChrom [((GST,Int),(GST,Int))]
  --           makeEdges' = do
  --               leaves <- map (\g -> (g,1)) <$> allGST
  --               let eq ((g,_),(f,_)) ((h,_),(j,_)) = (g,f) == (h,j)
  --
  --               rmdupsWith biggest eq <$> makeEdges 12 leaves
  --               where
  --                     biggest f@((_,i1),_) g@((_,j1),_)
  --                       | i1 > j1 = f
  --                       | otherwise = g


--
--
-- rmDupEdges :: [((GST,Int),(GST,Int))] -> [((GST,Int),(GST,Int))]
-- rmDupEdges = rmdupsWith f eq
--         where eq ((w,_),(x,_)) ((y,_),(z,_))
--                     = (w, x) == (y, z)
--               f (x,i) (y,j) = (addgst x y, i)

rmDupEdges :: Bool -- | True -> addNodes, False -> biggest
    -> [Edge]
    -> [Edge]
rmDupEdges p = rmdupsWith compare f (==)
    where f  (a,b) (x,y)
            | p         = (addNodes a x, y)
            | otherwise = ( a { nodeBasin = max (nodeBasin a) (nodeBasin x)
                              , nodeProperty = rmdups $ ((++) `on` nodeProperty) a x
                              }
                          , b
                          )

-- property :: GST -> String
-- property g
--     | g == startingGST = "start"
--     |
--


-- | Check the average fitness in the different environments after a number of updates
avgFitness :: Int -- | Number of updates
           -> [Env] -- | Which envirnments to check
           -> Parameters -- | How many things to check, needs sampleSize and seed
           -> AnalyzeChrom [Double] -- | The average hamming distances
avgFitness n es params = do
    rs <- randomGSTs params
    f  <- repeatApply n <$> nsf
    let updated = map f rs

    -- returns [[fitnesses e==0],[fitnesses e ==1], ...]
    let fitnesses = map (\e -> map (hammDist e) updated) es :: [[Int]]
    return $ map average fitnesses


developmentTime :: Parameters -> AnalyzeChrom Int
developmentTime params = do
    c <- ask
    let proto_a = emptyAgent {geneStateTable = toGST c, genome = [c]}
        a = if Landscapes.resetGeneStatesOnBirth params
            then setToStart proto_a
            else proto_a
    case trajectory a of
        Nothing -> return (-1)
        Just x  -> return $ length x

trajectory :: Agent -> Maybe [Agent]
trajectory = takeUntilSame . take 100 . iterate updateAgent
    where
        takeUntilSame :: [Agent] -> Maybe [Agent]
        takeUntilSame (a:b:rest)
            | sameGST a b = Just [a]
            | otherwise   = (a:) <$> takeUntilSame (b:rest)
        takeUntilSame _ = Nothing


divergencePointOfTrajectory :: Chromosome -> Chromosome -> Int
divergencePointOfTrajectory      c1 c2 = length $ takeWhile id $ zipWith (==)        g1 g2
    where g1 = fromJustDef [] $ map toGST <$> trajectory (setToStart $ agentFromChromosome c1)
          g2 = fromJustDef [] $ map toGST <$> trajectory (setToStart $ agentFromChromosome c2)
divergencePointOfTrajectoryOnOff :: Chromosome -> Chromosome -> Int
divergencePointOfTrajectoryOnOff c1 c2 = length $ takeWhile id $ zipWith equalsOnOFF g1 g2
    where g1 = fromJustDef [] $ map toGST <$> trajectory (setToStart $ agentFromChromosome c1)
          g2 = fromJustDef [] $ map toGST <$> trajectory (setToStart $ agentFromChromosome c2)
          equalsOnOFF gst1 gst2 = 0 == hammingDistance (f gst1) (f gst2)
            where f gst = map toOnOff $ M.elems gst

distanceAfterMutation :: Chromosome -> Chromosome -> Int
distanceAfterMutation before after = length $ fromJustDef [] $ trajectory beforeAgent
    where beforeAgent = (agentFromChromosome after) {geneStateTable = toGST before}

agentFromChromosome :: Chromosome -> Agent
agentFromChromosome c = emptyAgent {geneStateTable = toGST c, genome = [c]}


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

allCombinations' :: Integral a => [a] -> [[a]]
allCombinations' = mapM (enumFromTo 0)

-- | Not reversible with numToGST. Reduces all states to either 0 or 1
gstToNum :: AnalyzeChrom (GST -> Int)
gstToNum = do
    full <- toIntList <$> fullGST
    return $ \gst -> antiPickFromCombination (toIntList gst) full
        where --toOnOffList = map (getGeneState . toOnOff) . toGSL
              toIntList = map getGeneState . M.elems

-- | Converts a number to a GST
numToGST :: AnalyzeChrom (Int -> GST)
numToGST = do
    full <- fullGST
    -- return $ \i -> toGST $ pickFromCombination
    return $ \i -> toGST $ pickFromCombination i (toIntList full)
        where toIntList = map getGeneState . M.elems


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

$(deriveTextShow ''Node)
