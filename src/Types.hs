{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import Data.Map.Strict as Map
import Data.Array.IArray

type Time = Int
type Prob = Double

data World = World {    agents :: Agents
                    ,   env :: Env
                   } deriving (Show, Read, Eq)

type Env = Int
type Agents = Array (Int, Int) Agent
data Agent = Agent {    genome         :: Genome
                    ,   geneStateTable :: GeneStateTable
                    ,   born           :: (Env, Time)
                    ,   parent         :: Agent
                    ,   diff :: [Mutation]
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
                    ,   wt :: Weight } deriving (Show, Read, Eq, Ord)
instance GeneType Tfbs where iD = tfbsID

class GeneType a where
    iD :: a -> ID

newtype Thres     = Thres Int      deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype Weight    = Weight Int     deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype GeneState = GS Bool deriving  (Show, Read, Eq, Ord, Enum, Bounded)
instance Real GeneState where
    toRational (GS True)  = 1
    toRational (GS False) = 0
instance Num GeneState where
    GS a + GS b = GS $ a || b
    GS a * GS b = GS $ not $ a || b
    abs = id; signum = id;
    negate (GS a) = GS $ not a
    fromInteger a = if a > 0 then GS True else GS False

newtype ID = ID Int deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

data Mutation = GenDup   | GenDel     | GenThresCh |
                TfbsDup  | TfbsDel    | TfbsInnov  |
                TfbsWtCh | TfbsPrefCh
                deriving (Show, Eq, Read, Ord)
