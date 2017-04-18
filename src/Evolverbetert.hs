module Evolverbetert where

import           Control.Concurrent
import Control.Monad
import           Data.Array.IArray
import           Data.List
import           Data.List.Split      (splitEvery)
import qualified Data.Map             as Map
import           Misc
import           Mutations
import qualified Parameters           as P
import           System.Console.ANSI
-- import           MyRandom
import           World
import           Data.Function
import           System.IO
import Data.IORef
import Graphics.UI.GLUT hiding (mainLoop)
import Data.Fixed (mod')
import MyGraphics

import MyRandom

import qualified Control.Monad.Parallel as Par (mapM)
-- import Control.Monad.Random (getRandom, getRandomR, Rand, runRandT, getStdGen, setStdGen)


main :: IO ()
main = do

    let initialAgent = evalRand randomAgent (pureMT P.agent0Seed)
        initialWorld = (startAgents, 0)
            where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent

    print "The initial Agent is"
    setMyStdGen $ pureMT P.worldSeed

    worldRef <- newIORef initialWorld

    -- All GLUT related stuff
    when P.display $ do
        getArgsAndInitialize
        let pixelsPerUnit = 10
            w = pixelsPerUnit * fromIntegral P.width
            h = pixelsPerUnit * fromIntegral P.height
        initialDisplayMode $= [RGBMode, DoubleBuffered]
        initialWindowSize $= Size w h
        (Size screenSizeX screenSizeY) <- get screenSize
        let initialPos = Position
                (fromIntegral (screenSizeX - w) `div` 2)
                (fromIntegral (screenSizeY - h) `div` 2)
        initialWindowPosition $= initialPos
        createWindow "Evolverbetert v1.0"
        matrixMode $= Projection
        loadIdentity
        ortho2D 0 (fromIntegral w / fromIntegral pixelsPerUnit)
                0 (fromIntegral h / fromIntegral pixelsPerUnit)

        displayCallback $= showWorld worldRef

    -- Where the magic happens... Recursive function that ends on P.maxTime
    print "Hello, World!"
    mainLoop worldRef 0
    print "Goodbye World!"



mainLoop :: IORef World -> P.Time -> IO ()
mainLoop worldRef t | t == P.maxTime = return ()
mainLoop worldRef t = do
    w@(ags, env) <- readIORef worldRef

    when (t `mod` P.outputStep == 0)
        $ case P.outputMode of
            P.Console        -> consoleOutput w t
            P.File           -> fileOutput w t
            P.FileAndConsole -> consoleOutput w t >> fileOutput w t

    std <- getMyStdGen
    let (w',std') = runRand (newWorld w) std
    setMyStdGen std'


    writeIORef worldRef w'

    when P.display $
        mainLoopEvent >> postRedisplay Nothing

    mainLoop worldRef (t+1)

-- | It works like a clock
chEnv :: Env -> Rand Env
chEnv e = do
    r <- getRange (1, max 1 P.nrEnv-1)
    return $ (e + r) `mod` P.nrEnv

newWorld :: World -> Rand World
newWorld w@(ags,env) = do

    env' <- maybeCh env chEnv P.envSwitchProb
    -- Rand <- getMyStdRandom randomDouble :: IO Double
    -- env' <- maybeCh env chEnv P.envSwitchProb

    newAssocs <- mapM (newAssoc w) oldAssocs -- makes new association list
    let ags' = array P.worldBounds newAssocs
        w' = (ags', env') :: World
    return w'
        where oldAssocs = assocs ags

outputString :: World -> P.Time -> String
outputString w@(ags,env) t =
    show t
    ++ " " ++ show env
    ++ " " ++ show (minHammDist w)
    -- ++ " " ++ show (hammDist (Map.toList $ geneStateTable best) (Map.toList $ targetGST env) )
    -- ++ " " ++ show (showGST $ geneStateTable best)
    -- ++ " " ++ show (head $ genome best)
    where
        bestAgent :: World -> Agent
        bestAgent (ags, env) = maximumBy (compare `on` (`fitnessAgent` env)) $ elems ags
        maxFitness :: World -> Double
        maxFitness (ags, env) = maximum $ map (`fitnessAgent` env) (elems ags)
        minHammDist :: World -> Int
        minHammDist (ags,env) = minimum $ map (`hammDistAg` env) (elems ags)

fileOutput :: World -> P.Time -> IO ()
fileOutput w t = appendFile P.outputFile (outputString w t ++ "\n")

consoleOutput :: World -> P.Time -> IO ()
consoleOutput w t = putStrLn $ outputString w t

newAssoc :: World -> ((Int, Int), Agent) -> Rand ((Int, Int), Agent)
newAssoc w (ix, ag) = do
    ag' <- reproduceAgent w ix
    return (ix, ag')

displayWorld :: World -> IO ()
displayWorld w@(ags, _) = do
    let lijntjes = map concat $ splitEvery P.width $
            map agToChar $ elems ags where
                agToChar a = if a == NoAgent then " " else  "o"
    putStrLn $ unlines $ lijntjes

reproduceAgent :: World -> (Int, Int) -> Rand Agent
reproduceAgent (agents, env) ix = do
    temp1 <- getDouble
    if temp1 > P.deathRate then --if you survive
        if agents ! ix == NoAgent --if cell is empty, give every neighbour a weighted probability for getting chosen to reproduce
            then do
                temp2 <- getDouble
                let Just (_, iChooseYou) = find ((>=r) . fst) cumFitAg
                    neighbours = map (agents !) (moore8 ix) ++ [NoAgent] --list of the neighbours
                    fitnesses = map (`fitnessAgent` env) (init neighbours)
                                ++ [0.4^P.selectionPressure]  --list of fitnesses
                    cumFitnesses = scanl1 (+) fitnesses --cumulative list of fitnesses
                    cumFitAg = zip cumFitnesses neighbours --list of (cumfit, agent) pairs
                    r = temp2 * sum fitnesses

                iMutU <- mutAg iChooseYou
                if    iMutU /= iChooseYou
                 then return $ devAg iMutU
                 else return iMutU
        else return $ agents!ix
     else return NoAgent -- if you die
