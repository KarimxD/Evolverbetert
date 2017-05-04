module Mutations (
mutAg) where

import           Misc
import           Parameters
-- import Control.Monad.Random (getRandom, getRange, MonadRandom)
import           MyRandom
import Types

import           Control.Monad
import           Data.List
import qualified Parameters   as P (
      pTfbsDup, pTfbsDel, pTfbsWtCh, pTfbsInnov, pTfbsPrefCh
    , pGenDup, pGenDel, pGenThresCh
    )
import           World



dupChr :: Chromosome -> Rand Chromosome
dupChr = mutNet >=> mutateLoci >=> dupTfbss

_dupChr :: Chromosome -> Rand Chromosome
_dupChr = dupGenes >=>
          delGenes >=>
          dupTfbss

-- chGenThress :: Chromosome

dupTfbss :: Chromosome -> Rand Chromosome
dupTfbss c = do
    n <- binomial (length c) P.pTfbsDup
    repeatCollect n dupATfbs c

dupATfbs :: Chromosome -> Rand Chromosome
dupATfbs c = do
    let tfbss = reduceToTfbss [c]

    i1 <- getRange (0, length tfbss - 1)
    i2 <- getRange (0, length c     - 1)
    return $ let (h, t) = splitAt i2 c
              in  h ++ [CTfbs $ tfbss!!i1] ++ t


--
-- dupATfbs :: [Locus] -> Rand [Locus]
--
-- dupAGen [] = return []
-- dupAGen gs = do
--     let len = length gs
--     r1 <- getRange (0, len-1)
--     r2 <- getRange (0, len-1)
--     return $ let (h, t) = splitAt r1 gs
--               in  h ++ [gs!!r2] ++ t


dupGenes :: Chromosome -> Rand Chromosome
dupGenes c = do
    n <- binomial l P.pGenDup
    concat <$> repeatCollect n dupAGen gs
    where
        gs = groupGeneTfbs c
        l = length gs


delGenes :: Chromosome -> Rand Chromosome
delGenes c = do
    n <- binomial l P.pGenDel
    concat <$> repeatCollect n delAnElem gs
    where
        gs = groupGeneTfbs c
        l = length gs



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

-- Original Work from here:
mutNet :: [Locus] -> Rand [Locus]
mutNet c = do
    let genes'   = groupGeneTfbs c
        len = length genes'

    genes'' <- maybeCh genes'
        delAnElem (P.pGenDel * fromIntegral len)
    genes''' <- maybeCh genes''
        dupAGen (P.pGenDup * fromIntegral len)
    return $ concat genes'''

-- fix: Maybe doesn't ch
chGenThres :: Gene -> Rand Gene
chGenThres g = do
    r <- getRange (minThres, maxThres)
    return $ g {thres = r}

dupAGen :: [[Locus]] -> Rand [[Locus]]
dupAGen [] = return []
dupAGen gs = do
    let len = length gs
    r1 <- getRange (0, len-1)
    r2 <- getRange (0, len-1)
    return $ let (h, t) = splitAt r1 gs
              in  h ++ [gs!!r2] ++ t

delAnElem :: [a] -> Rand [a]
delAnElem [] = return []
delAnElem gs = do
    let len = length gs
    r <- getRange (0, len-1)
    return $ let (h, _:t) = splitAt r gs
              in  h ++ t

mutateLoci :: [Locus] -> Rand [Locus]
mutateLoci [] = return []
mutateLoci (h:rest) = case h of
    CGene g -> do
                rest' <- mutateLoci rest
                g' <- maybeCh g chGenThres pGenThresCh
                maybeTfbsList <- maybeCh [] innovateTfbs pTfbsInnov
                return $ map CTfbs maybeTfbsList ++ CGene g' : rest'
    CTfbs t -> do
                rest' <- mutateLoci rest
                t' <- maybeCh t chTfbsWt pTfbsWtCh
                t'' <- maybeCh t' chTfbsPref pTfbsPrefCh
                let list' = [t'']
                list'' <- maybeCh list' delAnElem pTfbsDel
                -- list' <- maybeCh list' innovateTfbs pTfbsInnov -- Fix for scaling to # of genes
                return $ map CTfbs list'' ++ rest'
    _       -> do
                rest' <- mutateLoci rest
                return $ h:rest'




innovateTfbs :: [Tfbs] -> Rand [Tfbs]
innovateTfbs list = do
    b       <- getBool
    let weight = if b then 1 else (-1)
    pref    <- randGeneType
    let tfbs = Tfbs pref weight
    return $ tfbs:list

chTfbsWt :: Tfbs -> Rand Tfbs
chTfbsWt t = return t { wt = newwt }
    where newwt = (-1) * wt t

-- fix: maybe doesn't ch
chTfbsPref :: Tfbs -> Rand Tfbs
chTfbsPref t = do
    r <- randGeneType
    return $ t { tfbsID = r }

mutAg :: Agent -> Rand Agent
mutAg NoAgent = return NoAgent
mutAg ag = do
    genome' <- mapM dupChr (genome ag)
    let gst' = gSTFromGenome genome'
    return (Agent genome' gst')

randGeneType :: Rand ID
randGeneType = ID <$> getRange (0, nrGeneTypes'-1)
