{-# LANGUAGE FlexibleInstances #-}
module Analysis where

import Types
import Parsing
import Misc (rmdups)
import qualified Data.Map.Strict as Map
import           Data.List.Split     (splitWhen)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)

type TaggedChromosome = [([Tfbs],TaggedGene)]
type AllTaggedGenes = Map.Map ID [TaggedGene]

type Edge = (TaggedGene, TaggedGene, Weight)
instance MyShow Edge where
    myShow (source,target,w) = myShow source ++"->"++myShow target ++ " " ++ myShow w

type Tag = Int
instance MyShow Tag where
    myShow = show

data TaggedGene = TaggedGene {      taggedGeneID :: ID
                    ,   tag :: Tag
                    ,   taggedthres :: Thres
                    ,   taggedGenSt :: GeneState } deriving (Show, Read, Eq)
instance Ord TaggedGene where
    compare x y = compare (taggedToGene x) (taggedToGene y)
        where taggedToGene (TaggedGene i _ th gs) = Gene i th gs

instance GeneType TaggedGene where
    iD = taggedGeneID

instance MyShow TaggedGene where
    myShow (TaggedGene i t th _) = concat [myShow i, ".", myShow t, ":", myShow th]

chromToEdges :: Chromosome -> [Edge]
chromToEdges c = concatMap (uncurry $ makeEdges table) $ concatMap pairAll taggedChromosome
    where taggedChromosome = tagChromosome c
          table = allTaggedGenes c

tagMe :: Tag -> Gene -> TaggedGene
tagMe t (Gene i th gs) = TaggedGene i t th gs

makeEdges :: AllTaggedGenes -> Tfbs -> TaggedGene -> [Edge]
makeEdges atgs tfbs tg = zip3 sources targets weights
    where i = iD tfbs
          sources = atgs Map.! i
          targets = repeat tg
          weights = repeat $ wt tfbs

tagChromosome :: Chromosome -> TaggedChromosome
tagChromosome c = concat result
    where groupedTfbss = map toTfbss $ splitWhen isGene c -- List of [Tfbs]
          genes = toGenes c -- List of Gene
          grouped = zip groupedTfbss genes -- List of ([Tfbs],Gene)
          sorted = sortBy (compare `on` snd) grouped -- List of ([Tfbs],Gene)
          henk = groupBy ((==) `on` iD . snd) sorted -- same as above grouped by geneID
          result = map (zipWith giveTag [0..]) henk  -- same as above converted Gene to TaggedGene

allTaggedGenes :: Chromosome -> AllTaggedGenes
allTaggedGenes = Map.fromList . zip [ID 0..] . groupSameID . map snd . tagChromosome
    where groupSameID = groupBy ((==) `on` iD)

giveTag :: Tag -> ([Tfbs],Gene) -> ([Tfbs], TaggedGene)
giveTag t = mapSnd (tagMe t)
    where mapSnd f (x, y) = (x, f y)

pairAll :: ([a], b) -> [(a,b)]
pairAll (xs, y) = map (\a -> (a,y)) xs

chromosomeToRNet :: Chromosome -> (String,String)
chromosomeToRNet c = (edges, nodes)
    where edges = chromosomeToEdgeFile c
          nodes = chromosomeToNodeFile c

chromosomeToEdgeFile :: Chromosome -> String
chromosomeToEdgeFile c =
    "SOURCE\tTARGET\tWEIGHT\n" ++ unlines edgelines
    where edges = chromToEdges c
          edgelines = map edgeToLine edges
          edgeToLine (src,tgt,w) = myShow src ++ "\t" ++ myShow tgt ++ "\t" ++ myShow w

chromosomeToNodeFile :: Chromosome -> String
chromosomeToNodeFile c =
    "ID\tON\tTarget\n" ++ unlines nodelines
    where nodes = concat $ Map.elems $ allTaggedGenes c
          nodelines = map taggedGeneToline nodes
          taggedGeneToline tg = myShow tg ++ "\t" ++ myShow (taggedGenSt tg) ++ "\t1"

chromosomeToDot :: Chromosome -> String
chromosomeToDot c =
    "// " ++ myShow c ++ "\n" ++
    (edgesToDot . chromToEdges) c

edgesToDot :: [Edge] -> String
edgesToDot edges =
       "digraph geneNetwork {\n"
    ++ concatMap (("    " ++) . edgeToDotline) edges
    ++ concatMap ("    " ++) (mapMaybe nodeToDotLine (edgesToNodes edges))
    ++ "}"

nodeToDotLine :: TaggedGene -> Maybe String
nodeToDotLine tg = if taggedGenSt tg > GS 0
                    then Just $ taggedToDot tg ++ style
                    else Nothing
                    where style = " [style = filled];\n"

edgesToNodes :: [Edge] -> [TaggedGene]
edgesToNodes = rmdups . map (\(_,tgt,_) -> tgt)

edgeToDotline :: Edge -> String
edgeToDotline (source,target,weight) =
    taggedToDot source ++ "->" ++ taggedToDot target ++ style
        where style = if weight > 0 then " [color=green];\n" else " [color=red];\n"

taggedToDot :: TaggedGene -> String
taggedToDot tg = "G" ++ myShow (taggedGeneID tg) ++ "x" ++ myShow (tag tg) ++ "x" ++ myShow (taggedGenSt tg)
