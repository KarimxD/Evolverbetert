{-# LANGUAGE FlexibleInstances #-}
module Parsing  where
import World
import Types
import           Data.List.Split     (splitOn)

import qualified Data.Map.Strict as Map
import qualified Data.List as List

class MyShow a where
    myShow :: a -> String

class MyRead a where
    myRead :: String -> a


instance MyShow Agent where
    myShow NoAgent = "NoAgent"
    myShow ag = show (concatMap myShow $ head $ genome ag, myShow $ geneStateTable ag) --Only works on agents with 1 chromosome
parseAgent :: String -> Agent
parseAgent "NoAgent" = NoAgent
parseAgent str =  --Only works on agents with 1 chromosome
    Agent genes gst 0 0 NoAgent []
    where
        gst = gSTFromGenome genes
        genes = [map myRead loci] :: Genome
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
    myShow (GS a) = show a
    -- myShow (GS True) = show (1 :: Int)
    -- myShow _         = show (0 :: Int)
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

instance MyShow Genome where
    myShow = myShow . concat
instance MyRead Genome where
    myRead s = [myRead s]

agentToLineageFile :: Agent -> String
agentToLineageFile = unlines . map (\(t,e,c,ms) -> List.intercalate ";" [show t, show e, myShow c, show ms])
    . reverse . agentToLineage

-- agentToLineage (Agent (chrom:_) _ (t,e) par _) = (t, e, chrom) : agentToLineage par
-- agentToLineage _ = []

agentToLineage :: Agent -> [(Time, Env, Chromosome, [Mutation])]
agentToLineage = map relevents . agentToLineageList
    where   relevents NoAgent = (0,0,[],[])
            relevents a = (bornTime a, bornEnv a, head (genome a), diff a)

agentToLineageList :: Agent -> [Agent]
agentToLineageList NoAgent = []
agentToLineageList a = a : agentToLineageList (parent a)
