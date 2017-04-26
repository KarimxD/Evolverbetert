module Parsing  where
import World
import Types
import           Data.List.Split     (splitOn)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Array.IArray as Array

instance Show Agent where
    show ag = show (concatMap show $ head $ genome ag, showGST $ geneStateTable ag) --Only works on agents with 1 chromosome
parseAgent str =  --Only works on agents with 1 chromosome
    Agent genome gst
    where
        gst = gSTFromGenome genome
        genome = [map parseLoc loci] :: Genome
        loci = splitOn "," str :: [String]

instance Show Locus where
    show Transposon = "T"
    show (CGene (Gene i t gs)) = "G" ++ show i ++ ":" ++ show t ++ ":" ++ show gs
    show (CTfbs (Tfbs i w)) = show i ++ ":" ++ show w
parseLoc :: String -> Locus
parseLoc str
    | h == 'G'   = CGene $ Gene (parseID $ tail $ head s) (parseThres $ s!!1) (parseGeneState $ s!!2)
    | str == "T" = Transposon
    | otherwise  = CTfbs $ Tfbs (parseID $ head s) (parseWeight $ s!!1)
        where h = head str; s = splitOn ":" str

showGST :: GeneStateTable -> String
showGST = List.intersperse ' ' . concatMap (show . snd) . Map.toList
readGST :: String -> GeneStateTable
readGST = Map.fromList . zip [0..] . map (parseGeneState . pure) . filter (/= ' ')

instance Show Thres where
    show (Thres i) = show i
parseThres :: String -> Thres
parseThres = Thres . read

instance Show Weight where
    show (Weight i) = show i
parseWeight :: String -> Weight
parseWeight = Weight . read

instance Show ID where
    show (ID i) = show i
parseID :: String -> ID
parseID = ID . read

instance Show GeneState where
    show (GS True) = show 1
    show (GS False)= show 0
parseGeneState :: String -> GeneState
parseGeneState "1" = GS True
parseGeneState _   = GS False

showAgents :: Agents -> String
showAgents = show . Array.assocs
