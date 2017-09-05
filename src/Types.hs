{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where
import qualified Data.Map.Strict as Map
import Data.Array.IArray
import Data.Maybe (mapMaybe, isJust)
import Data.Foldable as F (foldr')

type Time = Int
type Prob = Double

data World = World {    agents :: Agents
                    ,   env :: Env
                   } deriving (Show, Read, Eq)



type Env = Int
type Agents = Array (Int, Int) Agent
data Agent = Agent {    genome         :: Genome
                    ,   geneStateTable :: GeneStateTable
                    ,   bornTime       :: Time
                    ,   bornEnv        :: Env
                    ,   parent         :: Agent
                    ,   diff           :: [Mutation]
                   }
           | NoAgent deriving (Show, Read, Ord)
instance Eq Agent where
    NoAgent == NoAgent = True
    NoAgent == _ = False
    _ == NoAgent = False
    a == b = genome a == genome b

type Genome = [Chromosome]
type Chromosome = [Locus]
type GeneStateTable = Map.Map ID GeneState



data Locus
    = Transposon
    | CGene     Gene
    | CTfbs     Tfbs     deriving (Show, Read, Eq, Ord)
instance GeneType Locus where
    iD (CGene g) = iD g
    iD (CTfbs t) = iD t
    iD Transposon = -1

data Gene = Gene {      geneID :: ID
                    ,   thres :: Thres
                    ,   genSt :: GeneState } deriving (Show, Read, Eq)
instance Ord Gene where
    Gene i1 t1 gs1 `compare` Gene i2 t2 gs2 =
        if i1 == i2
        then if gs1 == gs2
             then compare t1 t2   --small thres first
             else compare gs2 gs1 --largest state firster
        else compare i1 i2        --smaller id firstest
instance GeneType Gene where iD = geneID

data Tfbs = Tfbs {      tfbsID :: ID
                    ,   wt :: Weight
                    ,   tfbsSt :: GeneState } deriving (Show, Read, Eq, Ord)
instance GeneType Tfbs where iD = tfbsID

class GeneType a where
    iD :: a -> ID

newtype Thres     = Thres Int      deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype Weight    = Weight Int     deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

-- newtype GeneState = GS Bool deriving  (Show, Read, Eq, Ord, Enum, Bounded)
-- instance Real GeneState where
--     toRational (GS True)  = 1
--     toRational (GS False) = 0
-- instance Num GeneState where
--     GS a + GS b = GS $ a || b
--     GS a * GS b = GS $ not $ a || b
--     abs = id; signum = id;
--     negate (GS a) = GS $ not a
--     fromInteger a = if a > 0 then GS True else GS False

newtype GeneState = GS Int deriving  (Show, Read, Ord, Enum, Real, Integral, Bounded)
instance Eq GeneState where
    GS 0 == GS 0 = True
    GS 0 == GS _ = False
    GS _ == GS 0 = False
    GS _ == GS _ = True
instance Num GeneState where
    GS a + GS b = GS $ a + b
    GS a * GS b = GS $ a * b
    abs = id; signum = id;
    negate (GS a) = GS $ -a
    fromInteger a = GS $ fromInteger a

newtype ID = ID Int deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)


data Mutation = GenDup ID   | GenDel ID    | GenThresCh ID |
                TfbsDup ID  | TfbsDel ID   | TfbsInnov ID  |
                TfbsWtCh ID | TfbsPrefCh ID
                deriving (Show, Eq, Read, Ord)

instance GeneType Mutation where
    iD (GenDup i) = i; iD (GenDel i) = i; iD (GenThresCh i) = i;
    iD (TfbsDup i) = i; iD (TfbsDel i) = i; iD (TfbsInnov i) = i;
    iD (TfbsWtCh i) = i; iD (TfbsPrefCh i) = i;


reduceChromToTfbss :: Chromosome -> [Tfbs]
reduceChromToTfbss = mapMaybe getTfbs

-- | Reduce a genome to a list of its transcription factor binding sites
reduceGenomeToTfbss :: Genome -> [Tfbs]
reduceGenomeToTfbss = reduceChromToTfbss . concat

reduceChromToGenes :: Chromosome -> [Gene]
reduceChromToGenes = mapMaybe getGene

-- | Reduce a genome to a list of its genes
reduceGenomeToGenes :: Genome -> [Gene]
reduceGenomeToGenes = reduceChromToGenes . concat

-- | Check whether a locus is a Gene or Tfbs
isGene, isTfbs :: Locus -> Bool
isGene = isJust . getGene
isTfbs = isJust . getTfbs

-- | Returns Just Gene if Locus is a Gene else Nothing
getGene :: Locus -> Maybe Gene
getGene (CGene g) = Just g
getGene _         = Nothing
-- | Returns Just Tfbs if Locus is a Tfbs else Nothing
getTfbs :: Locus -> Maybe Tfbs
getTfbs (CTfbs t) = Just t
getTfbs _         = Nothing

geneStateListToGST :: [GeneState] -> GeneStateTable
geneStateListToGST = Map.fromList . zip [0..]



class GST a where
    toGST   :: a -> GeneStateTable
    toGSL :: a -> [GeneState]
    toGSL = map snd . Map.toList . toGST

instance GST [GeneState] where
    toGST = Map.fromList . zip [0..]

instance GST GeneStateTable where
    toGST = id

instance GST Chromosome where
    toGST = gSTFromChrom where
        gSTFromChrom :: Chromosome -> GeneStateTable
        gSTFromChrom = makeGST . reduceChromToGenes
            where
                makeGST :: [Gene] -> GeneStateTable
                makeGST = F.foldr'
                    (\ !x !acc -> Map.insertWith (+)
                    (iD x) (genSt x) acc)
                    Map.empty

instance GST Genome where
    toGST = toGST . concat
instance GST Agent where
    toGST = geneStateTable
