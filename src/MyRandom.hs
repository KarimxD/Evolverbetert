module MyRandom
(
    Rand
    , PureMT
    , runRand
    , getModifyRand
    , evalRand
    , getBool
    , getDouble
    , getRange
    , getMyStdRandom
    , setMyStdGen
    , getMyStdGen
    , randomBool
    , pureMT
)   where

import           Control.Monad.State.Strict

import           Data.Int
import           Data.Word
import           System.Random                          (RandomGen, next, split)
import           System.Random.Mersenne.Pure64.Internal
import           System.Random.Mersenne.Pure64.MTBlock
-- import Data.Time.Clock
-- import Data.Time.Calendar
-- import System.CPUTime

import           Data.IORef                             (IORef,
                                                         atomicModifyIORef',
                                                         newIORef, readIORef,
                                                         writeIORef)
import           System.IO.Unsafe                       (unsafePerformIO)


type Rand = State PureMT

runRand :: State PureMT a -> PureMT -> (a, PureMT)
runRand = runState
{-# INLINE runRand #-}

evalRand :: State PureMT a -> PureMT -> a
evalRand = evalState
{-# INLINE evalRand #-}

getModifyRand :: Rand PureMT
getModifyRand = do
    it <- get
    let (_,new) = next it
    put new
    return it
{-# INLINE getModifyRand #-}

getBool :: Rand Bool
getBool = state randomBool
{-# INLINE getBool #-}

getDouble :: Rand Double
getDouble = state randomDouble
{-# INLINE getDouble #-}

getRange :: (Int,Int) -> Rand Int
getRange = state . randomR
{-# INLINE getRange #-}


-- Rand2


-- Edited by Karim Hajji for ease of use
-- {-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne.Pure64
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.3
--
-- A purely functional binding 64 bit binding to the classic mersenne
-- twister random number generator. This is more flexible than the
-- impure 'mersenne-random' library, at the cost of being a bit slower.
-- This generator is however, many times faster than System.Random,
-- and yields high quality randoms with a long period.
--
-- This generator may be used with System.Random, however, that is
-- likely to be slower than using it directly.



------------------------------------------------------------------------





-- | Create a PureMT generator from an Integral seed.
pureMT :: Integral a => a -> PureMT
pureMT = mkPureMT . seedBlock . fromIntegral

-- #if !MIN_VERSION_time(1, 6, 0)
-- diffTimeToPicoseconds :: DiffTime -> Integer
-- diffTimeToPicoseconds d =
--     round (1000 * 1000 * 1000 * 1000 * d)
-- #endif

-- | Create a new PureMT generator, using the clocktime as the base for the seed.
newPureMT :: IO PureMT
newPureMT = return $ pureMT 42
 -- do
 --    ct <- getCPUTime
 --    t  <- getCurrentTime
 --    let seed = toModifiedJulianDay (utctDay t) + diffTimeToPicoseconds (utctDayTime t) + ct
 --    return $ pureMT $ fromIntegral seed

------------------------------------------------------------------------
-- System.Random interface.

-- $instance
--
-- Being purely functional, the PureMT generator is an instance of
-- RandomGen. However, it doesn't support 'split' yet.

instance RandomGen PureMT where
   next  = randomInt
   split = splitPureMT --error "System.Random.Mersenne.Pure: unable to split the mersenne twister"

splitPureMT :: PureMT -> (PureMT, PureMT)
splitPureMT g = (pureMT s, pureMT s') where
 (s', g'') = randomWord64 g'
 (s , g' ) = randomWord64 g

------------------------------------------------------------------------
-- Direct access to Int, Word and Double types

-- | Yield a new 'Int' value from the generator, returning a new
-- generator and that 'Int'. The full 64 bits will be used on a 64 bit machine.
randomInt :: PureMT -> (Int, PureMT)
randomInt g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomInt #-}


-- | Yield a new 'Word' value from the generator, returning a new
-- generator and that 'Word'.
randomWord :: PureMT -> (Word, PureMT)
randomWord g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomWord #-}


-- | Yield a new 'Int64' value from the generator, returning a new
-- generator and that 'Int64'.
randomInt64 :: PureMT -> (Int64, PureMT)
randomInt64 g = (fromIntegral i, g')
        where (i, g') = randomWord64 g
{-# INLINE randomInt64 #-}

-- | Efficiently yield a new 53-bit precise 'Double' value, and a new generator.
randomDouble :: PureMT -> (Double, PureMT)
randomDouble g = (fromIntegral (i `div` 2048) / 9007199254740992, g')
        where (i, g') = randomWord64 g
{-# INLINE randomDouble #-}

-- | Yield a new 'Word64' value from the generator, returning a new
-- generator and that 'Word64'.
randomWord64 :: PureMT -> (Word64, PureMT)
randomWord64 (PureMT block i nxt) = (mixWord64 (block `lookupBlock` i), mt)
  where
    mt | i < blockLen-1 = PureMT block (i+1) nxt
       | otherwise      = mkPureMT nxt
{-# INLINE randomWord64 #-}

randomBool :: PureMT -> (Bool, PureMT)
randomBool g = (0.5 < d, g')
    where (d, g') = randomDouble g
{-# INLINE randomBool #-}


-- create a new PureMT from an MTBlock
mkPureMT :: MTBlock -> PureMT
mkPureMT block = PureMT block 0 (nextBlock block)


--------------------------------------------------------------------MyShit


randomR :: (Int, Int) -> PureMT -> (Int, PureMT)
randomR (lo, hi) g =
    let (d, g') = randomDouble g
        r = lo + floor (d * fromIntegral (hi-lo+1))
    in if r < lo || r > hi
            then (lo,g') --error $ "lo: " ++ show lo ++ " hi: " ++ show hi ++ " r: " ++ show r ++ " d: " ++ show d --  (0,g') --randomR (lo,hi) g'
            else (r,g')

setMyStdGen :: PureMT -> IO ()
setMyStdGen = writeIORef theMyStdGen

-- |Gets the global random number generator.
getMyStdGen :: IO PureMT
getMyStdGen  = readIORef theMyStdGen


{-# NOINLINE theMyStdGen #-}
theMyStdGen :: IORef PureMT
theMyStdGen  = unsafePerformIO $
   newIORef $ pureMT 42


getMyStdRandom :: (PureMT -> (a, PureMT)) -> IO a
getMyStdRandom f = atomicModifyIORef' theMyStdGen (swap . f)
    where swap (v, g) = (g, v)
