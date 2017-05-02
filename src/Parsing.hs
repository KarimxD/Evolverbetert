{-# LANGUAGE FlexibleInstances #-}
module Parsing  where
import World
import Types
import           Data.List.Split     (splitOn)

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Array.IArray as Array

class MyShow a where
    myShow :: a -> String

class MyRead a where
    myRead :: String -> a


instance MyShow Agent where
    myShow NoAgent = "NoAgent"
    myShow ag = show (concatMap myShow $ head $ genome ag, myShow $ geneStateTable ag) --Only works on agents with 1 chromosome
parseAgent "NoAgent" = NoAgent
parseAgent str =  --Only works on agents with 1 chromosome
    Agent genome gst
    where
        gst = gSTFromGenome genome
        genome = [map myRead loci] :: Genome
        loci = splitOn "," str :: [String]

instance MyShow GeneStateTable where
    myShow = List.intersperse ' ' . concatMap (myShow . snd) . Map.toList
instance MyRead GeneStateTable where
    myRead = Map.fromList . zip [0..] . map (myRead . pure) . filter (/= ' ')

instance MyShow ID where
    myShow (ID i) = show i
instance MyRead ID where
    myRead = ID . read

instance MyShow Thres where
    myShow (Thres i) = show i
instance MyRead Thres where
    myRead = Thres . read

instance MyShow Weight where
    myShow (Weight i) = show i
instance MyRead Weight where
    myRead = Weight . read

instance MyShow GeneState where
    myShow (GS True) = show (1 :: Int)
    myShow _         = show (0 :: Int)
instance MyRead GeneState where
    myRead = fromInteger . read

instance MyShow Locus where
    myShow Transposon = "T"
    myShow (CGene (Gene i t gs)) = "G" ++ myShow i ++ ":" ++ myShow t ++ ":" ++ myShow gs
    myShow (CTfbs (Tfbs i w)) = myShow i ++ ":" ++ myShow w
instance MyRead Locus where
    myRead str
        | h == 'G'   = CGene $ Gene (myRead $ tail $ head s) (myRead $ s!!1) (myRead $ s!!2)
        | str == "T" = Transposon
        | otherwise  = CTfbs $ Tfbs (myRead $ head s) (myRead $ s!!1)
             where h = head str; s = splitOn ":" str

instance MyShow Chromosome where
    myShow loci = List.intercalate "," (map myShow loci)
instance MyRead Chromosome where
    myRead = map myRead . splitOn ","


-- making dot files
genomeToDot :: Genome -> String
genomeToDot g =
       "digraph geneNetwork {"
    ++ chromosomeToDot (concat g)
    ++ "}"

chromosomeToDot :: Chromosome -> String
chromosomeToDot c =
    concatMap groupedToDot (groupGeneTfbs c)

groupedToDot :: [Locus] -> String
groupedToDot loci = tfbssGeneToDot gene tfbss
 where
    tfbss = reduceToTfbss [loci]
    gene = case last loci of
        CGene g -> g
        _ -> undefined

sortofmap :: [a] -> b -> (a->b->c) -> [c]
sortofmap xs y f = map (`f` y) xs

tfbssGeneToDot :: Gene -> [Tfbs] -> String
tfbssGeneToDot g = concatMap (geneTfbsToDot g)

geneTfbsToDot :: Gene -> Tfbs -> String
geneTfbsToDot g t = it ++ "->" ++ ig ++ ";"
    where
        it = "T" ++ myShow (CTfbs t)
        ig = myShow (iD g) ++ ".1"
