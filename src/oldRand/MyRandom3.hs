module MyRandom3
(
    Rand
    , runRand
    , getBool
    , getDouble
    , getRange
    , getMyStdRandom
    , setMyStdGen
    , getMyStdGen
)   where

import MyRandom2
import Control.Monad.State


type Rand = State PureMT

runRand :: State PureMT a -> PureMT -> (a, PureMT)
runRand = runState

getBool :: Rand Bool
getBool = state randomBool

getDouble :: Rand Double
getDouble = state randomDouble

getRange :: (Int,Int) -> Rand Int
getRange = state . randomR
