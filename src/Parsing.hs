{-# LANGUAGE FlexibleInstances #-}
module Parsing  where
import Types
import           Data.List.Split     (splitOn)

import Safe

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as C

class MyShow a where
    myShow :: a -> String

class MyRead a where
    myRead :: String -> a
    myRead = fromJust . readMaybe
    readMaybe :: String -> Maybe a


instance MyShow Agent where
    myShow NoAgent = "NoAgent"
    myShow ag = show (concatMap myShow $ head $ genome ag, myShow $ geneStateTable ag) --Only works on agents with 1 chromosome
parseAgent :: String -> Agent
parseAgent "NoAgent" = NoAgent
parseAgent str =  --Only works on agents with 1 chromosome
    Agent genes gst 0 0 NoAgent []
    where
        gst = toGST genes
        genes = [map myRead loci] :: Genome
        loci = splitOn "," str :: [String]

instance MyShow GeneStateTable where
    myShow = List.intersperse ' ' . concatMap (myShow . snd) . Map.toList
instance MyRead GeneStateTable where
    myRead = Map.fromList . zip [0..] . map (myRead . pure) . filter (/= ' ')


instance MyShow ID where
    myShow (ID i) = if i < 10
                  then '0' : show i
                  else show i
instance MyRead ID where
    readMaybe = fmap ID . readMay

instance MyShow Thres where
    myShow (Thres i) = show i
instance MyRead Thres where
    readMaybe = fmap Thres . readMay

instance MyShow Weight where
    myShow (Weight i) = show i
instance MyRead Weight where
    readMaybe = fmap Weight . readMay

instance MyShow GeneState where
    myShow (GS a) = show a
    -- myShow (GS True) = show (1 :: Int)
    -- myShow _         = show (0 :: Int)
instance MyRead GeneState where
    readMaybe = fmap fromInteger . readMay

instance MyShow Locus where
    myShow Transposon = "T"
    myShow (CGene (Gene i t gs)) = "G" ++ myShow i ++ ":" ++ myShow t ++ ":" ++ myShow gs
    myShow (CTfbs (Tfbs i w _)) = myShow i ++ ":" ++ myShow w
instance MyRead Locus where
    readMaybe str
        | h == 'G'   = CGene <$> (Gene <$> (readMaybe =<< tailMay =<< headMay s) <*> (readMaybe =<< s `atMay` 1) <*> (readMaybe =<< s `atMay` 2)
                                    )
        | str == "T" = Just Transposon
        | otherwise  = fmap CTfbs $ Tfbs <$> (readMaybe =<< headMay s)
                                         <*> (readMaybe =<< s `atMay` 1)
                                         <*> Just 0
             where h = head str; s = splitOn ":" str

instance MyShow Chromosome where
    myShow loci = List.intercalate "," (map myShow loci)
instance MyRead Chromosome where
    readMaybe = parseList . map readMaybe . splitOn ","

instance MyShow Genome where
    myShow = myShow . concat
instance MyRead Genome where
    readMaybe s = parseList [readMaybe s]

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

cRead :: Read a => C.ByteString -> a
cRead = read . C.unpack

cMyRead :: MyRead a => C.ByteString -> a
cMyRead = myRead . C.unpack

cShow :: Show a => a -> C.ByteString
cShow = C.pack . show

cMyShow :: MyShow a => a -> C.ByteString
cMyShow = C.pack . myShow

parseList :: [Maybe a] -> Maybe [a]
parseList [] = Just []
parseList (Nothing:_)    = Nothing
parseList (x:xs) = x `maybeAppend` parseList xs

maybeAppend :: Maybe a -> Maybe [a] -> Maybe [a]
maybeAppend (Just x) (Just xs) = Just $ x : xs
maybeAppend _ _ = Nothing
