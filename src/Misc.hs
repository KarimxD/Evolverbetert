module Misc (
      valueResultPairs
    , maybeCh
    , moore8
    , repeatCollect)
    where

import           Data.Array.IO
import           Parameters    as P
import MyRandom
import Control.Monad


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
