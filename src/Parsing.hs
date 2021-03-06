{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Parsing  where
import Types
import           Data.List.Split     (splitOn)

import Safe

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.String

class IsString a => ToString a where
    toString   :: a -> String

instance ToString C.ByteString where
    toString   = C.unpack
instance ToString T.Text where
    toString   = T.unpack


class MyShow a where
    myShow  :: a -> String
    myShow' :: IsString s => a -> s
    myShow' = fromString . myShow

class MyRead a where
    myRead :: String -> a
    myRead = fromJust . readMaybe
    readMaybe :: String -> Maybe a
    readMaybe' :: ToString s => s -> Maybe a
    readMaybe' = readMaybe . toString

instance MyShow Agent where
    myShow NoAgent = "NoAgent"
    myShow ag = show (concatMap myShow $ head $ genome ag, myShow $ geneStateTable ag) --Only works on agents with 1 chromosome
parseAgent :: String -> Agent
parseAgent "NoAgent" = NoAgent
parseAgent str =  --Only works on agents with 1 chromosome
    emptyAgent { genome = genes, geneStateTable = gst}
    -- Agent genes gst 0 0 NoAgent [] False Map.empty
    where
        gst = toGST genes
        genes = [map myRead loci] :: Genome
        loci = splitOn "," str :: [String]

instance MyShow GST where
    myShow = unwords . map myShow . Map.elems
instance MyRead GST where -- FIX: return Nothing if fails
    readMaybe = Just . Map.fromList . zip [0..] . map (myRead . pure) . filter (/= ' ')

instance MyShow ID where
    myShow (ID i) = if i < 10 then '0' : show i else show i
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
instance MyRead GeneState where
    readMaybe = fmap fromInteger . readMay

instance MyShow Gene where
    myShow (Gene i t gs) = "G" ++ myShow i ++ ":" ++ myShow t ++ ":" ++ myShow gs
instance MyRead Gene where
    readMaybe str = Gene <$> (readMaybe =<< tailMay =<< headMay s)
                         <*> (readMaybe =<< s `atMay` 1)
                         <*> (readMaybe =<< s `atMay` 2)
        where s = splitOn ":" str

instance MyShow Tfbs where
    myShow (Tfbs i w _) = myShow i ++ ":" ++ myShow w
instance MyRead Tfbs where
    readMaybe str = Tfbs <$> (readMaybe =<< headMay s)
                         <*> (readMaybe =<< s `atMay` 1)
                         <*> Just 0
        where s = splitOn ":" str

instance MyShow Locus where
    -- myShow Transposon = "T"
    myShow (CGene g) = myShow g
    myShow (CTfbs t) = myShow t
instance MyRead Locus where
    readMaybe str
        | h == 'G'   = CGene <$> readMaybe str
        -- str == "T" = Just Transposon
        | otherwise  = CTfbs <$> readMaybe str
             where h = head str


instance MyShow Chromosome where
    myShow loci = List.intercalate "," (map myShow loci)
instance MyRead Chromosome where
    readMaybe = parseList . map readMaybe . splitOn ","

instance MyShow Genome where
    myShow = myShow . concat
instance MyRead Genome where
    readMaybe s = parseList [readMaybe s]

instance (MyShow a) => MyShow (Maybe a) where
    myShow (Just x) = "Just " ++ myShow x
    myShow Nothing  = "Nothing"

instance (MyRead a) => MyRead (Maybe a) where
    readMaybe ('J':'u':'s':'t':' ':xs) = readMaybe xs
    readMaybe _        = Nothing


agentToLineageFile :: Agent -> String
agentToLineageFile = unlines . map (\(t,e,c,ms) -> List.intercalate ";" [show t, show e, myShow c, show ms])
    . reverse . agentToLineage

-- agentToLineage (Agent (chrom:_) _ (t,e) par _) = (t, e, chrom) : agentToLineage par
-- agentToLineage _ = []

agentToLineage :: Agent -> [(Time, Env, Chromosome, [Mutation])]
agentToLineage = map relevants . agentToLineageList
    where   relevants NoAgent = (0,0,[],[])
            relevants a = (bornTime a, bornEnv a, head (genome a), diff a)

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

tRead :: Read a => T.Text -> a
tRead = read . T.unpack

tMyRead :: MyRead a => T.Text -> a
tMyRead = myRead . T.unpack

tMyShow :: MyShow a => a -> T.Text
tMyShow = T.pack . myShow

parseList :: [Maybe a] -> Maybe [a]
parseList [] = Just []
parseList (Nothing:_)    = Nothing
parseList (x:xs) = x `maybeAppend` parseList xs

maybeAppend :: Maybe a -> Maybe [a] -> Maybe [a]
maybeAppend (Just x) (Just xs) = Just $ x : xs
maybeAppend _ _ = Nothing

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
    pure = return
    Just' f <*> Just' x = Just' (f x)
    _ <*> _ = Nothing'

instance Monad Maybe' where
    Nothing' >>= _ = Nothing'
    Just' a >>= f = f a
    return = Just'
