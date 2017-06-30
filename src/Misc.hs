module Misc (
      valueResultPairs
    , maybeCh
    , moore8
    , repeatCollect
    , roundToNearest
    , rmdups)
    where

import           Control.Monad
import           MyRandom
import           Parameters    as P
import qualified Data.Set as Set


rmdups :: Ord a => [a] -> [a]
rmdups = Set.toList . Set.fromList
-- rmdups = rmdups' Set.empty where
--   rmdups' _ [] = []
--   rmdups' a (b : c) = if Set.member b a
--     then rmdups' a c
--     else b : rmdups' (Set.insert b a) c



moore8 :: (Int, Int) -> [(Int, Int)]
moore8 (i, j) = filter (not . outOfBounds)
    [(i-1, j-1), (i, j-1), (i+1, j-1),
     (i-1, j)  ,           (i+1, j),
     (i-1, j+1), (i, j+1), (i+1, j+1)]

outOfBounds :: (Int, Int) -> Bool
outOfBounds (x, y) = x < 0 || y < 0 || x >= P.width || y >= P.height

valueResultPairs :: (a->b) -> [a]  -> [(a, b)]
valueResultPairs f = map (\x -> (x, f x))

-- hammDist :: Eq a => [a] -> [a] -> Int
-- hammDist (a:as) (b:bs) = if a /= b then 1 + hammDist as bs else hammDist as bs
-- hammDist _ _ = 0
--hammDist as bs = length $ filter (==True) $ zipWith (/=) as bs

maybeCh :: a -> (a -> Rand a) -> Double -> Rand a
maybeCh x f p = do
    r <- getDouble
    if r < p
        then f x
        else return x
{-# INLINE maybeCh #-}

-- | Repeats a monadic operation n times collecting the results
repeatCollect :: Monad m => Int -> (a -> m a) -> a -> m a
repeatCollect n f = foldr (<=<) return $ replicate n f

roundToNearest :: Integral a => a -> a -> a
roundToNearest i n = n - n `rem` i
