module Landscapes
    where

import Types
-- import qualified Parameters as P
import Misc (valueResultPairs)
import World (updateChrom)
import Data.Bifunctor
-- import Control.Parallel.Strategies
-- import Data.Vector.Mutable as V
-- import qualified Data.Map.Strict as M
-- defaultAgent :: Agent
-- defaultAgent = Agent [] defaultGst undefined undefined
--
-- nrAttr :: Agent -> Int
-- nrAttr = length . filter id . zipWith (==) [0..] . attractorLandscape
--
-- attractorLandscape :: Agent -> [Integer]
-- attractorLandscape a = map (gst2dec . newGst) states
--     where
--         newGst :: GeneStateTable -> GeneStateTable
--         newGst gst = geneStateTable $ updateAgent $ a {geneStateTable = gst}
--         states = map dec2gst [0..2^n-1]
--         n = P.nrGeneTypes'


attrNum :: Chromosome -> Int
attrNum = length . --withStrategy (parList rdeepseq) .
    filter id . map (uncurry (==) . bimap (fmap toOnOff) (fmap toOnOff)) .
    vertices

vertices :: Chromosome -> [(GeneStateTable,GeneStateTable)]
vertices c = valueResultPairs (nsf c) $ allGST c

nsf :: Chromosome -> GeneStateTable -> GeneStateTable
nsf c = toGST . flip updateChrom c

--
-- developpedGST :: GeneStateTable -> Agent -> GeneStateTable
-- developpedGST gst ag = geneStateTable $ devAg' $ setAgent gst ag
--
-- setAgent :: GeneStateTable -> Agent -> Agent
-- setAgent gst ag = ag { geneStateTable = gst }

turnLocusOn :: Locus -> Locus
turnLocusOn (CTfbs t) = CTfbs $ t {tfbsSt = GS 1}
turnLocusOn (CGene g) = CGene $ g {genSt  = GS 1}

turnGeneOn :: Gene -> Gene
turnGeneOn g = g {genSt  = GS 1}

fullGST :: Chromosome -> GeneStateTable
fullGST = toGST . map (onGene turnGeneOn)


allGST :: GST gst => gst -> [GeneStateTable]
allGST = map toGST . allCombinations . toGSL


allGSTofChrom :: Chromosome -> [GeneStateTable]
allGSTofChrom = allGST . fullGST

prependAll :: [a] -> [[a]] -> [[a]]
prependAll [] _ = []
prependAll (x:xs) ls = map (x:) ls ++ prependAll xs ls
-- prependAll = fix ((`ap` tail) . (. head) . flip ((.) . ap . ((++) .) . map . (:)))

allCombinations :: Integral a => [a] -> [[a]]
allCombinations = foldr (\ n -> prependAll [0 .. n]) [[]]

toOnOff :: GeneState -> GeneState
toOnOff (GS 0) = GS 0
toOnOff _      = GS 1


agent42 :: Agent
agent42 = read "Agent {genome = [[CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 2, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 8, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 0, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 7, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 16, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 5, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 10, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 13, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 14, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 18, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 9, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 17, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 19, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 3, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 11, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 12, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 15, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 1, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 4, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 6, thres = Thres 0, genSt = GS 0})]], geneStateTable = fromList [(ID 0,GS 0),(ID 1,GS 0),(ID 2,GS 0),(ID 3,GS 0),(ID 4,GS 0),(ID 5,GS 1),(ID 6,GS 0),(ID 7,GS 0),(ID 8,GS 1),(ID 9,GS 1),(ID 10,GS 0),(ID 11,GS 1),(ID 12,GS 0),(ID 13,GS 0),(ID 14,GS 1),(ID 15,GS 0),(ID 16,GS 0),(ID 17,GS 0),(ID 18,GS 1),(ID 19,GS 0)], bornTime = 0, bornEnv = 0, parent = NoAgent, diff = []}"
