{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import Data.Map as Map
import Data.List as List
import Data.Array.IArray

data World = World {    agents :: Agents
                    ,   env :: Env}

type Env = Int
type Agents = Array (Int, Int) Agent
data Agent = Agent {    genome         :: Genome
                    ,   geneStateTable :: GeneStateTable}
           | NoAgent deriving (Eq, Ord)

type Genome = [Chromosome]
type Chromosome = [Locus]
type GeneStateTable = Map.Map ID GeneState
data Locus
    = Transposon
    | CGene     Gene
    | CTfbs     Tfbs     deriving (Eq, Ord)




data Gene = Gene {      geneID :: ID
                    ,   thres :: Thres
                    ,   genSt :: GeneState } deriving (Eq)
instance Ord Gene where
    Gene i1 t1 gs1 `compare` Gene i2 t2 gs2 =
        if i1 == i2
        then if gs1 == gs2
             then compare t1 t2   --small thres first
             else compare gs2 gs1 --largest state firster
        else compare i1 i2        --smaller id firstest
instance GeneType Gene where iD = geneID

data Tfbs = Tfbs {      tfbsID :: ID
                    ,   wt :: Weight } deriving (Eq, Ord)
instance GeneType Tfbs where iD = tfbsID

class GeneType a where
    iD :: a -> ID

newtype Thres     = Thres Int      deriving  (Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype Weight    = Weight Int     deriving  (Eq, Ord, Real, Num, Enum, Integral, Bounded)

--
-- newtype Thres = Thres Int         deriving (Show, Read, Eq, Ord)
-- newtype GeneState = GeneState Bool deriving (Show, Read, Eq, Ord)--deriving (Show, Read, Eq, Ord)
-- newtype Weight = Weight Int       deriving (Show, Read, Eq, Ord)



newtype GeneState = GS Bool deriving  (Eq, Ord, Enum, Bounded)
instance Real GeneState where
    toRational (GS True)  = 1
    toRational (GS False) = 0
instance Num GeneState where
    GS a + GS b = GS $ a || b
    GS a * GS b = GS $ not $ a || b
    abs = id; signum = id;
    negate (GS a) = GS $ not a
    fromInteger a = if a > 0 then GS True else GS False


newtype ID = ID Int deriving  (Eq, Ord, Real, Num, Enum, Integral, Bounded)
-- instance Enum ID where toEnum = ID; fromEnum (ID a) = a
-- instance Real ID where toRational (ID a) = toRational a
-- instance Integral ID where
--     quotRem (ID a) (ID b) = (\(x,y) -> (ID x,ID y)) $ quotRem a b
--     toInteger (ID a) = toInteger a
-- instance Num ID where
--     ID a + ID b     = ID $ a + b
--     ID a * ID b     = ID $ a * b
--     abs (ID a)      = ID $ abs a
--     signum (ID a)   = ID $ signum a
--     fromInteger a   = ID $ fromInteger a
--     negate (ID a)   = ID $ negate a