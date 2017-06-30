module Landscapes
    where

import World
import Parameters as P
import Types
import Misc
import qualified Data.Map as M

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
