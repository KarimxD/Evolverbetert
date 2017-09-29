{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- A list of types and their conversions
--
-----------------------------------------------------------------------------



module Types where
import qualified Data.Map.Strict as Map
-- import Data.Array.IArray
import Data.Array.Unboxed
import Data.Maybe (mapMaybe, isJust)
import Data.Foldable as F (foldr')
import Data.Bifunctor

type Time = Int
type Prob = Double
type Seed = Int

data World = World {    agents :: Agents
                    ,   env :: Env
                   } deriving (Show, Read, Eq)

type Env = Int
type Agents = Array (Int, Int) Agent
data Agent = Agent {    genome         :: Genome
                    ,   geneStateTable :: GST
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
type GST = Map.Map ID GeneState
type GST' = Array ID GeneState

data CLocus a b
    = CTfbs     a
    | CGene     b   deriving (Show, Read, Ord)
instance (Eq a, Eq b) => Eq (CLocus a b) where
    CTfbs t1 == CTfbs t2 = t1 == t2
    CGene t1 == CGene t2 = t1 == t2
    _ == _ = False
instance Bifunctor CLocus where
    bimap f _ (CTfbs t) = CTfbs $ f t
    bimap _ f (CGene g) = CGene $ f g
onGene :: (Gene -> Gene) -> Locus -> Locus
onGene = second
onTfbs :: (Tfbs -> Tfbs) -> Locus -> Locus
onTfbs = first

type Locus = CLocus Tfbs Gene
instance GeneType Locus where
    iD (CGene g) = iD g
    iD (CTfbs t) = iD t

data Gene = Gene {      geneID :: ID
                    ,   thres :: Thres
                    ,   genSt :: GeneState } deriving (Show, Read, Eq)
instance Ord Gene where
    Gene i1 t1 gs1 `compare` Gene i2 t2 gs2 =
        if | i1 == i2  -> if | gs1 == gs2 -> compare t1 t2
                             | otherwise  -> compare gs2 gs1
           | otherwise -> compare i1 i2
        -- if i1 == i2               --smaller id first
        -- then if gs1 == gs2        --largest state second
        --      then compare t1 t2   --small thres third
        --      else compare gs2 gs1
        -- else compare i1 i2
instance GeneType Gene where iD = geneID
addGenes :: Gene -> Gene -> Gene
addGenes g1 g2 = g1 {genSt = genSt g1 + genSt g2}

data Tfbs = Tfbs {      tfbsID :: ID
                    ,   wt :: Weight
                    ,   tfbsSt :: GeneState } deriving (Show, Read, Eq, Ord)
instance GeneType Tfbs where iD = tfbsID

class GeneType a where
    iD :: a -> ID

newtype Thres     = Thres Int      deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype Weight    = Weight Int     deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded)

newtype GeneState = GS Int deriving  (Show, Read, Ord, Enum, Real, Integral, Bounded, Num)
instance Eq GeneState where
    GS 0 == GS 0 = True
    GS 0 == GS _ = False
    GS _ == GS 0 = False
    GS _ == GS _ = True
getGeneState :: GeneState -> Int
getGeneState (GS i) = i
-- instance Num GeneState where
--     GS a + GS b = GS $ a + b
--     GS a * GS b = GS $ a * b
--     abs = id; signum = id;
--     negate (GS a) = GS $ -a
--     fromInteger a = GS $ fromInteger a

newtype ID = ID Int deriving  (Show, Read, Eq, Ord, Real, Num, Enum, Integral, Bounded, Ix)


data Mutation = GenDup ID   | GenDel ID    | GenThresCh ID |
                TfbsDup ID  | TfbsDel ID   | TfbsInnov ID  |
                TfbsWtCh ID | TfbsPrefCh ID
                deriving (Show, Eq, Read, Ord)

instance GeneType Mutation where
    iD (GenDup i) = i; iD (GenDel i) = i; iD (GenThresCh i) = i;
    iD (TfbsDup i) = i; iD (TfbsDel i) = i; iD (TfbsInnov i) = i;
    iD (TfbsWtCh i) = i; iD (TfbsPrefCh i) = i;

class InferTfbss a where
    toTfbss :: a -> [Tfbs]
class InferGenes a where
    toGenes :: a -> [Gene]
instance InferTfbss Chromosome where
    toTfbss = mapMaybe getTfbs
instance InferTfbss Genome where
    toTfbss = toTfbss . concat
instance InferGenes Chromosome where
    toGenes = mapMaybe getGene
instance InferGenes Genome where
    toGenes = toGenes . concat

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

class InferGST a where
    toGST   :: a -> GST
    toGSL :: a -> [GeneState]
    toGSL = Map.elems . toGST

instance InferGST [Int] where
    toGST = toGST . map GS

instance InferGST [GeneState] where
    toGST = Map.fromList . zip [0..]

instance InferGST GST where
    toGST = id

instance InferGST Chromosome where
    toGST = gSTFromChrom where
        gSTFromChrom :: Chromosome -> GST
        gSTFromChrom = makeGST . toGenes
            where
                makeGST :: [Gene] -> GST
                makeGST = F.foldr'
                    (\ !x !acc -> Map.insertWith (+)
                    (iD x) (genSt x) acc)
                    Map.empty

instance InferGST Genome where
    toGST = toGST . concat

instance InferGST Agent where
    toGST = geneStateTable


-- New Shyt
-- class InferGST' a where
--     toGST'   :: a -> GST'
--     toGSL' :: a -> [GeneState]
--     toGSL' = elems . toGST'
--
-- instance InferGST' [GeneState] where
--     toGST' xs = listArray bounds' xs
--         where bounds' = (ID 0,ID $ length xs)
--
-- instance InferGST' GST' where
--     toGST' = id
--
-- instance InferGST' Chromosome where
--     toGST' = gSTFromChrom where
--         gSTFromChrom :: Chromosome -> GST'
--         gSTFromChrom = makeGST . toGenes
--             where
--                 makeGST :: [Gene] -> GST'
--                 makeGST gs = genSt <$> accum addGenes (listArray bounds' []) assocs'
--                     where bounds' = (ID 0, iD $ maximum gs)
--                           assocs' = map (\g -> (iD g, g)) gs
--
--                     -- F.foldr'
--                     -- (\ !x !acc -> Map.insertWith (+)
--                     -- (iD x) (genSt x) acc)
--                     -- Map.empty
--
-- instance InferGST' Genome where
--     toGST' = toGST' . concat
-- -- instance InferGST' Agent where
--     -- toGST' = geneStateTable
