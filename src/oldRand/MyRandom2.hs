
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

module MyRandom2 --where

    -- where {}
    --
    --
(

    -- * The random number generator
    PureMT          -- abstract: RandomGen

    -- * Introduction
    , pureMT        -- :: Word64 -> PureMT
    , newPureMT     -- :: IO PureMT

    -- $instance

    -- * Low level access to the generator

    -- $notes
    , randomInt     -- :: PureMT -> (Int   , PureMT)
    , randomWord    -- :: PureMT -> (Word  , PureMT)
    , randomInt64   -- :: PureMT -> (Int64 , PureMT)
    , randomWord64  -- :: PureMT -> (Word64, PureMT)
    , randomDouble  -- :: PureMT -> (Double, PureMT)
    , randomBool    -- :: PureMT -> (Double, PureMT)
    , getMyStdRandom
    , setMyStdGen
    , getMyStdGen
    , randomR
    ) where

------------------------------------------------------------------------

import System.Random.Mersenne.Pure64.MTBlock
import System.Random.Mersenne.Pure64.Internal
import System.Random (RandomGen, next, split)
import Data.Word
import Data.Int
-- import Data.Time.Clock
-- import Data.Time.Calendar
-- import System.CPUTime

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef' )
import System.IO.Unsafe ( unsafePerformIO )



-- | Create a PureMT generator from a 'Word64' seed.
pureMT :: Word64 -> PureMT
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

-- | Yield a new 'Word' value from the generator, returning a new
-- generator and that 'Word'.
randomWord :: PureMT -> (Word, PureMT)
randomWord g = (fromIntegral i, g')
        where (i, g') = randomWord64 g

-- | Yield a new 'Int64' value from the generator, returning a new
-- generator and that 'Int64'.
randomInt64 :: PureMT -> (Int64, PureMT)
randomInt64 g = (fromIntegral i, g')
        where (i, g') = randomWord64 g

-- | Efficiently yield a new 53-bit precise 'Double' value, and a new generator.
randomDouble :: PureMT -> (Double, PureMT)
randomDouble g = (fromIntegral (i `quot` 2048) / 9007199254740992, g')
        where (i, g') = randomWord64 g
-- {-# INLINE randomDouble #-}

-- | Yield a new 'Word64' value from the generator, returning a new
-- generator and that 'Word64'.
randomWord64 :: PureMT -> (Word64, PureMT)
randomWord64 (PureMT block i nxt) = (mixWord64 (block `lookupBlock` i), mt)
  where
    mt | i < blockLen-1 = PureMT block (i+1) nxt
       | otherwise      = mkPureMT nxt
-- {-# INLINE randomWord64 #-}

randomBool :: PureMT -> (Bool, PureMT)
randomBool g = (0.5 < d, g')
    where (d, g') = randomDouble g

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



{-


module MyRandom where
import Control.Monad.Random
import qualified System.Random.Mersenne.Pure64 as M
import Data.IORef       ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef' )
import Data.Int
import System.IO.Unsafe ( unsafePerformIO )
import GHC.Word


randomR :: (Int, Int) -> MyPureMT -> (Int, MyPureMT)
randomR (lo, hi) g =
    let (d, g') = random g :: (Double, MyPureMT)
    in (floor $ fromIntegral lo + d * fromIntegral (hi-lo), g')

setMyStdGen :: MyPureMT -> IO ()
setMyStdGen = writeIORef theMyStdGen

-- |Gets the global random number generator.
getMyStdGen :: IO MyPureMT
getMyStdGen  = readIORef theMyStdGen

theMyStdGen :: IORef MyPureMT
theMyStdGen  = unsafePerformIO $
   newIORef $ myPureMT 42

getMyStdRandom :: (MyPureMT -> (a, MyPureMT)) -> IO a
getMyStdRandom f = atomicModifyIORef' theMyStdGen (swap . f)
    where swap (v, g) = (g, v)
--
-- randomInt :: MyPureMT -> (Int, MyPureMT)
-- randomInt = myPureMT . randomInt
--
-- randomWord :: MyPureMT -> (Word, MyPureMT)
--
--
-- randomInt64 :: MyPureMT -> (Int64, MyPureMT)
--
--
-- randomWord64 :: MyPureMT -> (Word64, MyPureMT)

-- | Efficiently yield a new 53-bit precise 'Double' value, and a new generator.
randomDouble :: MyPureMT -> (Double, MyPureMT)
randomDouble g = (fromIntegral (i `div` 2048) / 9007199254740992, g')
        where (i, g') = randomWord64 g
{-# INLINE randomDouble #-}

-- | Yield a new 'Word64' value from the generator, returning a new
-- generator and that 'Word64'.
randomWord64 :: MyPureMT -> (Word64, MyPureMT)
randomWord64 (MyPureMT (M.PureMT block i nxt)) = (mixWord64 (block `lookupBlock` i), mt)
  where
    mt | i < blockLen-1 = PureMT block (i+1) nxt
       | otherwise      = mkPureMT nxt
{-# INLINE randomWord64 #-}




newtype MyPureMT = MyPureMT { unMyPureMT :: M.PureMT }
myPureMT = MyPureMT . M.pureMT

instance RandomGen MyPureMT where
  next  = nextMyPureMT
  split = splitMyPureMT

splitMyPureMT :: MyPureMT -> (MyPureMT, MyPureMT)
splitMyPureMT (MyPureMT g) = (myPureMT s, myPureMT s') where
  (s', g'') = M.randomWord64 g'
  (s , g' ) = M.randomWord64 g

nextMyPureMT (MyPureMT g) = (s, MyPureMT g') where
  (s, g') = M.randomInt g
-}
