{-# OPTIONS_GHC -w #-}

module MyRandom
(
    Rand (..)
    -- , R (..)
    , PureMT
    , runRand
    -- , runRandom
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
import           Control.Monad (liftM, ap)
import           Data.Int
import           Data.Word
import           System.Random                          (RandomGen, next, split, random, getStdRandom)
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

data Mutation = GenDup | GenDel | GenThresCh
                | TfbsWtCh | TfbsPrefCh
                | TfbsDel | TfbsInnov | TfbsDup

type Rand = State (PureMT, [Mutation])

-- data R a = R !a {-# UNPACK #-}!PureMT
{-
-- | A basic random monad, for generating random numbers from pure mersenne twisters.
newtype Rand a = Rand { runRand :: PureMT -> R a }

instance Functor Rand where
    fmap = liftM

instance Applicative Rand where
    {-# INLINE pure #-}
    pure a = Rand $ \s -> R a s
    (<*>) = ap

instance Monad Rand where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    m >>= k  = Rand $ \s -> case runRand m s of
                                R a s' -> runRand (k a) s'
    {-# INLINE (>>) #-}
    m >>  k  = Rand $ \s -> case runRand m s of
                                R _ s' -> runRand k s'

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRandom  :: Rand a -> PureMT -> (a, PureMT)
runRandom r g = case runRand r g of R x g -> (x, g)

-- | Evaluate a random computation using the mersenne generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRand :: Rand a -> PureMT -> a
evalRand r g = case runRand r g of R x _ -> x

getModifyRand :: Rand PureMT
getModifyRand = Rand $ \s -> case randomWord64 s of (w,s') -> R s s'

getRange :: Integral a => (Int,Int) -> Rand a
getRange range = Rand $ \s -> case randomR range s of (w,s') -> R (fromIntegral w) s'

-- {-# INLINE getRange #-}

    -- _ <- getDouble
    -- it <- get
    -- let (_,new) = next it
    -- put new
    -- return it

------------------------------------------------------------------------
-- Efficient 'get' functions.


getBool     :: Rand Bool
getBool     = Rand $ \s -> case randomInt s of (w,s') -> R (w < 0) s'

-- | Yield a new 'Int' value from the generator.
getInt      :: Rand Int
getInt      = Rand $ \s -> case randomInt s of (w,s') -> R w s'

-- | Yield a new 'Word' value from the generator.
getWord     :: Rand Word
getWord     = Rand $ \s -> case randomWord s of (w,s') -> R w s'

-- | Yield a new 'Int64' value from the generator.
getInt64    :: Rand Int64
getInt64    = Rand $ \s -> case randomInt64 s of (w,s') -> R w s'

-- | Yield a new 53-bit precise 'Double' value from the generator.
getDouble   :: Rand Double
getDouble   = Rand $ \s -> case randomDouble s of (w,s') -> R w s'

-- | Yield a new 'Word64' value from the generator.
getWord64   :: Rand Word64
getWord64   = Rand $ \s -> case randomWord64 s of (w,s') -> R w s'


-}

--old monad

runRand :: State PureMT a -> PureMT -> (a, PureMT)
runRand = runState
-- {-# INLINE runRand #-}

evalRand :: State PureMT a -> PureMT -> a
evalRand = evalState
-- {-# INLINE evalRand #-}

getModifyRand :: Rand PureMT
getModifyRand = do
    (pmt,ls) <- get
    let (_,new) = next pmt
    put (new,ls)
    return pmt
-- {-# INLINE getModifyRand #-}

getBool :: Rand Bool
getBool = state randomBool'
    where
        randomBool' (pmt,lst) =
            (\(d,p)->(d,(p,lst))) $ randomBool pmt
-- {-# INLINE getBool #-}


getDouble :: Rand Double
-- getDouble = return $ unsafePerformIO $ getStdRandom random
getDouble = state randomDouble'
    where
        randomDouble' (pmt,lst) =
            (\(d,p)->(d,(p,lst))) $ randomDouble pmt
-- {-# INLINE getDouble #-}

getRange :: Integral a => (Int,Int) -> Rand a
getRange = fmap fromIntegral . state . randomR'
    where
        randomR' (i1,i2) (pmt,lst) =
            (\(i,p) -> (i,(p,lst))) $ randomR (i1,i2) pmt
-- {-# INLINE getRange #-}


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

-- -- | Create a new PureMT generator, using the clocktime as the base for the seed.
-- newPureMT :: IO PureMT
-- newPureMT = return $ pureMT 42
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
