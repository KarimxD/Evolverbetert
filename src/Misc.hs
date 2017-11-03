module Misc
    ( valueResultPair
    , valueResultPairs
    , moore8
    , repeatCollect
    , bottomToNearest
    , rmdups
    , histogram
    , count
    , counts
    , rotate
    , rectangulate
    , horizontalHistogram
    , verticalHistogram
    , mapIfPred
    , takeEvery
    , sample
    , listHead
    , pickFromCombination
    , antiPickFromCombination
    , repeatApply
    , slice
    , average
    )

    where

import           Control.Monad
import           Parameters    as P
import qualified Data.Set as Set
import           Data.List (transpose, genericLength)



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
valueResultPairs = map . valueResultPair

valueResultPair :: (a->b) -> a -> (a, b)
valueResultPair f x = (x, f x)

-- | Repeats a monadic operation n times collecting the results
repeatCollect :: Monad m => Int -> (a -> m a) -> a -> m a
repeatCollect n f = foldr (<=<) return $ replicate n f

repeatApply :: Int -> (a->a) -> a -> a
repeatApply n f = foldr (.) id $ replicate n f

bottomToNearest :: Integral a => a -> a -> a
bottomToNearest i n = n - n `rem` i


-- Histogram stuff

verticalHistogram :: (Show a, Ord a) => [a] -> String
verticalHistogram = unlines . rotate .
    rectangulate ' ' . lines . horizontalHistogram

horizontalHistogram :: (Show a, Ord a) => [a] -> String
horizontalHistogram = unlines . map (\(a,n) -> show a ++ "═" ++ replicate n '▮')
    . histogram

histogram :: (Ord a) => [a] -> [(a,Int)]
histogram xs = counts (rmdups xs) xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- | counts "abc" "aacccb" = [('a',2),('b',1),('c',3)]
counts :: Eq a => [a] -> [a] -> [(a, Int)]
counts = flip (valueResultPairs . flip count)

rotate :: [[a]] -> [[a]]
rotate =  reverse . transpose

rectangulate :: a -> [[a]] -> [[a]]
rectangulate pad rows = newrows
    where
        width' = maximum $ map length rows
        infiniterows = map (++ repeat pad) rows
        newrows = map (take width') infiniterows

mapIfPred :: Functor f => (a -> Bool) -> (a -> a) -> f a -> f a
mapIfPred p f = fmap f'
    where f' x = if p x then f x else x

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = listHead ys ++ takeEvery n zs where (ys,zs) = splitAt n xs

listHead :: [a] -> [a]
listHead [] = []
listHead (x:_) = [x]

sample :: Int -> [a] -> [a]
sample i xs = takeEvery n xs
    where n = case length xs `div` i of
                0 -> 1
                x -> x

pickFromCombination :: Int -> [Int] -> [Int]
pickFromCombination _ [] = []
pickFromCombination 0 (_:xs) = 0 : pickFromCombination 0 xs
pickFromCombination n (_:xs) = n' : pickFromCombination r xs
    where p = product $ map (+1) xs
          (n', r) = n `quotRem` p

antiPickFromCombination :: [Int] -> [Int] -> Int
antiPickFromCombination [] [] = 0
antiPickFromCombination (n:ns) (f:fs) = n' + antiPickFromCombination ns fs
    where p  = product $ map (+1) (f:fs)
          n' = n * (p `div` (f+1))
antiPickFromCombination _ _ = error "nonmatching lists"

slice :: Int -> Int -> [a] -> [a]
slice i j = take (j-i) . drop i

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs
