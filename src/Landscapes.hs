module Landscapes
    where

import Types
import qualified Parameters as P
import Misc (rmdups)
import World (defaultGst, updateAgent, devAg')
-- import Data.Vector.Mutable as V
import qualified Data.Map as M

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
--
--
-- bin2dec :: [Integer] -> Integer
-- bin2dec = sum . zipWith (*) [2^n | n <- [0..]] . reverse
--
-- dec2bin :: Integer -> [Integer]
-- dec2bin = reverse . dec2bin'
--   where
--     dec2bin' 0 = []
--     dec2bin' y = let (a,b) = quotRem y 2 in b : dec2bin' a
--
-- gst2dec :: GeneStateTable -> Integer
-- gst2dec = bin2dec . gst2bin
--
-- dec2gst :: Integer -> GeneStateTable
-- dec2gst = bin2gst . dec2bin
--
-- gst2bin :: GeneStateTable -> [Integer]
-- gst2bin = map fromIntegral . M.keys
--
-- bin2gst :: [Integer] -> GeneStateTable
-- bin2gst = M.fromList . zip [0..] . map fromInteger




attractorLandscape :: Agent -> [GeneStateTable]
attractorLandscape ag = rmdups $ map (`developpedGST` ag) possibleGeneStateTables

developpedGST :: GeneStateTable -> Agent -> GeneStateTable
developpedGST gst ag = geneStateTable $ devAg' $ setAgent gst ag

setAgent :: GeneStateTable -> Agent -> Agent
setAgent gst ag = ag { geneStateTable = gst }

possibleGeneStateTables :: [GeneStateTable]
possibleGeneStateTables =
    map (listToGST . num2bin) [0..2^n-1]
    where
        n = fromIntegral P.nrGeneTypes'

listToGST :: [Int] -> GeneStateTable
listToGST ls = M.fromList $ zip [0..] $ map fromIntegral $ myZip ls defaultList
    where
        defaultList = replicate 0 n
        n = fromIntegral P.nrGeneTypes

-- | conserves first list and adds second
myZip :: [a] -> [a] -> [a]
myZip (x:xs) (_:ys) = x : myZip xs ys
myZip [] [] = []
myZip (x:xs) [] = x : myZip xs []
myZip [] (y:ys) = y : myZip ys []

num2bin :: Integral a => a -> [a]
num2bin 0 = []
num2bin n = num2bin (n `div` 2) ++ [n `mod` 2]
