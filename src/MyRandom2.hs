module MyRandom2 where

import System.IO.Unsafe ( unsafePerformIO )
import System.Random.MWC
import Control.Monad.Primitive
import Data.IORef
import Control.Monad.ST


-- type Rand = State (Gen a)

runRand :: (forall s . ST s a) -> a
runRand = runST



-- type Rand = State PureMT
--
-- runRand :: State PureMT a -> PureMT -> (a, PureMT)
-- runRand = runState
-- {-# INLINE runRand #-}
--
-- evalRand :: State PureMT a -> PureMT -> a
-- evalRand = evalState
-- {-# INLINE evalRand #-}
--
-- getModifyRand :: Rand PureMT
-- getModifyRand = do
--     it <- get
--     let (_,new) = next it
--     put new
--     return it
-- {-# INLINE getModifyRand #-}
--
-- getBool :: Rand Bool
-- getBool = state randomBool
-- {-# INLINE getBool #-}
--
-- getDouble :: Rand Double
-- getDouble = state randomDouble
-- {-# INLINE getDouble #-}
--
-- getRange :: (Int,Int) -> Rand Int
-- getRange = state . randomR
-- {-# INLINE getRange #-}



-- setMyStdGen :: Gen () -> IO ()
setMyStdGen = writeIORef theMyStdGen

-- |Gets the global random number generator.
-- getMyStdGen :: PrimMonad m => IO (Gen (PrimState m))
getMyStdGen  = readIORef theMyStdGen

{-# NOINLINE theMyStdGen #-}


-- theMyStdGen :: IORef a
theMyStdGen = unsafePerformIO $ do
    henk <- create -- initialize $ singleton 42
    newIORef henk
