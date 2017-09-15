{-# LANGUAGE BangPatterns #-}

module Mutations (
mutAg) where

import           Misc (repeatCollect)
import           Parameters
-- import Control.Monad.Random (getRandom, getRange, MonadRandom)
import           MyRandom
import           Types

import           Control.Monad
import           Control.Monad.Writer
import           Data.List
import           World



dupChr :: Chromosome -> Mut Chromosome
dupChr = mutNet >=> mutateLoci >=> dupTfbss

-- _dupChr :: Chromosome -> Rand Chromosome
-- _dupChr = dupGenes >=>
--           delGenes >=>
--           dupTfbss

-- chGenThress :: Chromosome

dupTfbss :: Chromosome -> Mut Chromosome
dupTfbss c = do
    n <- lift $ binomial (length c) pTfbsDup

    repeatCollect n dupATfbs c
dupATfbs :: Chromosome -> Mut Chromosome
dupATfbs c = do
    let tfbss = reduceChromToTfbss c

    i1 <- lift $ getRange (0, length tfbss - 1)
    i2 <- lift $ getRange (0, length c     - 1)
    let (h, t) = splitAt i2 c
        loc = CTfbs $ tfbss!!i1 :: Locus
    tell [TfbsDup $ iD loc]
    return $ h ++ [loc] ++ t



-- dupATfbs :: [Locus] -> Rand [Locus]
--
-- dupAGen [] = return []
-- dupAGen gs = do
--     let len = length gs
--     r1 <- getRange (0, len-1)
--     r2 <- getRange (0, len-1)
--     return $ let (h, t) = splitAt r1 gs
--               in  h ++ [gs!!r2] ++ t


-- dupGenes :: Chromosome -> Rand Chromosome
-- dupGenes c = do
--     n <- binomial l pGenDup
--     concat <$> repeatCollect n dupAGen gs
--     where
--         gs = groupGeneTfbs c
--         l = length gs


-- delGenes :: Chromosome -> Rand Chromosome
-- delGenes c = do
--     n <- binomial l pGenDel
--     concat <$> repeatCollect n delAnElem gs
--     where
--         gs = groupGeneTfbs c
--         l = length gs



-- | A poisson distribution would be more accurate, but this is a (very) good approximation
binomial :: Int -> Prob -> Rand Int
binomial n''' p = do
    rand <- getDouble
    case findIndex (>rand) cumDist of
        Just a  -> return a
        Nothing -> return 0
    where
        n = fromIntegral n''' :: Int
        cumDist = scanl1 (+) dist
        dist = map binom [0..n]
        binom k = fromIntegral (n `choose` k) * p^k * (1-p)^(n-k)
        choose n' k' = foldl (\z i -> (z * (n'-i+1)) `div` i) 1 [1..k']
        -- choose n' k' = product [k'+1..n'] `quot` product [1..n'-k']

-- binomial' n p = cumDist
--     where
--         cumDist = scanl1 (+) dist
--         dist = map binom [0..n]
--         binom k = fromIntegral (n `choose` k) * p^k * (1-p)^(n-k)
--         choose n' k' = product [k'+1..n'] `quot` product [1..n'-k']


mutNet :: [Locus] -> Mut [Locus]
mutNet c = do
    let genes   = groupGeneTfbs c
        len = length genes

    genes' <- maybeChLog genes
        delAGen (pGenDel * fromIntegral len)
    genes'' <- maybeChLog genes'
        dupAGen (pGenDup * fromIntegral len)
    return $ concat genes''

-- fix: Maybe doesn't ch
chGenThres :: Gene -> Mut Gene
chGenThres !g = do
    r <- lift $ getRange (minThres, maxThres)
    tell [GenThresCh $ geneID g]
    return $ g {thres = r}

dupAGen :: [[Locus]] -> Mut [[Locus]]
dupAGen [] = return []
dupAGen gs = do
    let len = length gs
    r1 <- lift $ getRange (0, len-1)
    r2 <- lift $ getRange (0, len-1)
    let (h, t) = splitAt r1 gs
        duplicated = gs!!r2
    tell [GenDup $ iD $ last duplicated]
    return $ h ++ [duplicated] ++ t
--
-- delAnElem :: [a] -> Mut [a]
-- delAnElem [] = return []
-- delAnElem gs = do
--     let len = length gs
--     r <- lift $ getRange (0, len-1)
--     return $ let (h, _:t) = splitAt r gs
--               in  h ++ t

delATfbs :: [Tfbs] -> Mut [Tfbs]
delATfbs [] = return []
delATfbs gs = do
    let len = length gs
    r <- lift $ getRange (0, len-1)
    let (h, deleted:t) = splitAt r gs
    tell [TfbsDel $ iD deleted]
    return $ h ++ t

delAGen :: [[Locus]] -> Mut [[Locus]]
delAGen [] = return []
delAGen gs = do
    let len = length gs
    r <- lift $ getRange (0, len-1)
    let (h, deleted:t) = splitAt r gs
    tell [GenDel $ iD $ last deleted]
    return $ h ++ t

mutateLoci :: [Locus] -> Mut [Locus]
mutateLoci [] = return []
mutateLoci (h:rest) = case h of
    CGene !g -> do
                rest' <- mutateLoci rest
                g' <- maybeChLog g chGenThres pGenThresCh
                maybeTfbsList <- maybeChLog [] innovateTfbs pTfbsInnov
                return $ map CTfbs maybeTfbsList ++ CGene g' : rest'

    CTfbs !t -> do
                rest' <- mutateLoci rest
                t' <- maybeChLog t chTfbsWt pTfbsWtCh
                t'' <- maybeChLog t' chTfbsPref pTfbsPrefCh
                let list' = [t'']
                list'' <- maybeChLog list' delATfbs pTfbsDel
                -- list' <- maybeCh list' innovateTfbs pTfbsInnov -- Fix for scaling to # of genes
                return $ map CTfbs list'' ++ rest'
    --
    -- _       -> do
    --             rest' <- mutateLoci rest
    --             return $ h:rest'




innovateTfbs :: [Tfbs] -> Mut [Tfbs]
innovateTfbs list = do
    b       <- lift getBool
    let weight = if b then 1 else (-1)
    pref    <- lift randGeneType
    let tfbs = Tfbs pref weight 0

    tell [TfbsInnov $ iD tfbs]
    return $ tfbs:list

chTfbsWt :: Tfbs -> Mut Tfbs
chTfbsWt !t = do
    tell [TfbsWtCh $ iD t]
    return t { wt = newwt }
        where newwt = (-1) * wt t


-- fix: maybe doesn't ch
chTfbsPref :: Tfbs -> Mut Tfbs
chTfbsPref !t = do
    r <- lift randGeneType
    tell [TfbsPrefCh r]
    return $ t { tfbsID = r }

-- | mutates agent according to 'Parameters'
mutAg :: Agent -> Rand Agent
mutAg NoAgent = return NoAgent
mutAg !ag = do
    (genome', mutations) <- runWriterT $ mapM dupChr (genome ag)
    let gst' = toGST genome'
    return $ ag { genome = genome',
                  geneStateTable = gst',
                  diff = mutations }

randGeneType :: Rand ID
randGeneType = ID <$> getRange (0, nrGeneTypes'-1)

-- type Mut a = Writer [Mutation] (Rand a)
type Mut a = WriterT [Mutation] Rand a


maybeChLog :: a -> (a -> Mut a) -> Double -> Mut a
maybeChLog x f p = do
    r <- lift getDouble
    if r < p
        then f x
        else return x


--ID <$> getRange (0, nrGeneTypes'-1)
