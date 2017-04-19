
module Evolverbetert where
import           Misc                   (maybeCh, moore8)
import           Mutations              (mutAg)
import           MyGraphics             (showWorld)
import           MyRandom
import qualified Parameters             as P
import           World

import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Graphics.UI.GLUT       hiding (Help, initialize, mainLoop)
import           System.Console.GetOpt
import           System.Environment     (getArgs)

import           Control.Monad          (when)
import           Data.Fixed             (mod')
import           Data.Function          (on)
import           Data.Maybe             (fromMaybe)

import           Data.Array.IArray      (array, assocs, elems, (!))
import           Data.List              (find, maximumBy)
import           Data.List.Split        (splitEvery)
import qualified Data.Map               as Map

import qualified Control.Monad.Parallel as Par (mapM)

data Flag
    = Help
    | OutputFile String
    | WorldSeed String
    | AgentSeed String
    | Graphics
    deriving (Eq, Ord, Show)
isHelp        Help          = True; isHelp _       = False
isOutputFile (OutputFile _) = True; isOutputFile _ = False
isWorldSeed  (WorldSeed  _) = True; isWorldSeed _  = False
isAgentSeed  (AgentSeed  _) = True; isAgentSeed _  = False
isGraphics    Graphics      = True; isGraphics _    = False


options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]        (NoArg Help)                   "display this help info"
    , Option ['w'] ["world-seed"]  (ReqArg WorldSeed "INT")       "give the seed for the world RNG (default: 420)"
    , Option ['a'] ["agent-seed"]  (ReqArg AgentSeed "INT")       "give the seed for the first agent RNG (default: 420)"
    , Option ['o'] ["output-file"] (ReqArg OutputFile "FILEPATH") "output file"
    , Option ['g'] ["graphics"]    (NoArg Graphics)               "display CA in a window"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: ic [OPTION...] files..."

initialize :: [Flag] -> IO (IORef World)
initialize flags = do
    let needHelp  = Help `elem` flags
        WorldSeed sWorldSeed = fromMaybe (WorldSeed "420") (find isWorldSeed flags)
        AgentSeed sAgentSeed = fromMaybe (AgentSeed "420") (find isAgentSeed flags)
        worldSeed = read sWorldSeed
        agentSeed = read sAgentSeed

    when needHelp $ ioError $ userError ('\n': usageInfo header options)

    let initialAgent = evalRand randomAgent (pureMT agentSeed)
        initialWorld = World startAgents 0
            where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent

    print $ "The initial Agent is: " ++ show initialAgent
    setMyStdGen $ pureMT worldSeed

    newIORef initialWorld
        where header = "Usage: Evolverbetert [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    (flags, strings) <- compilerOpts args

    worldRef <- initialize flags


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

type StatsRef = IORef World

newWorld' :: StatsRef -> World -> World
newWorld' = undefined

mainLoop :: IORef World -> P.Time -> IO ()
mainLoop worldRef t | t == P.maxTime = return ()
mainLoop worldRef t = do
    w <- readIORef worldRef

    when (t `mod'` P.outputStep == 0)
        $ case P.outputMode of
            P.Console        -> consoleOutput w t
            P.File           -> fileOutput w t
            P.FileAndConsole -> consoleOutput w t >> fileOutput w t

    -- henk w


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
    return $ (e + r) `mod'` P.nrEnv

-- | Very ugly deepseq
henk :: World -> IO ()
henk w = do
    h <- newIORef $ Agent [[]] Map.empty
    writeIORef h $ maximumBy (compare `on` (`fitnessAgent` env w)) $ elems (agents w)
    readIORef h
    return ()
{-# NOINLINE henk #-}

newWorld :: World -> Rand World
newWorld w = do
    e' <- maybeCh e chEnv P.envSwitchProb

    newAssocs <- mapM (newAssoc w) oldAssocs -- makes new association list
    let ags' = array P.worldBounds newAssocs
        w' = World {agents = ags', env = e'}
    return w'
        where oldAssocs = assocs $ agents w
              e = env w

outputString :: World -> P.Time -> String
outputString w@(World ags env) t =
    show t
    ++ " " ++ show env
    ++ " " ++ show (minHammDist w)
    -- ++ " " ++ show (hammDist (Map.toList $ geneStateTable best) (Map.toList $ targetGST env) )
    -- ++ " " ++ show (showGST $ geneStateTable best)
    -- ++ " " ++ show (head $ genome best)
    where
        bestAgent :: World -> Agent
        bestAgent (World ags env) = maximumBy (compare `on` (`fitnessAgent` env)) $ elems ags
        maxFitness :: World -> Double
        maxFitness (World ags env) = maximum $ map (`fitnessAgent` env) (elems ags)
        minHammDist :: World -> Int
        minHammDist (World ags env) = minimum $ map (`hammDistAg` env) (elems ags)

fileOutput :: World -> P.Time -> IO ()
fileOutput w t = appendFile P.defaultOutputFile (outputString w t ++ "\n")

consoleOutput :: World -> P.Time -> IO ()
consoleOutput w t = putStrLn $ outputString w t

newAssoc :: World -> ((Int, Int), Agent) -> Rand ((Int, Int), Agent)
newAssoc w (ix, ag) = do
    ag' <- reproduceAgent w ix
    return (ix, ag')

reproduceAgent :: World -> (Int, Int) -> Rand Agent
reproduceAgent (World ags env) ix = do
    temp1 <- getDouble
    if temp1 > P.deathRate --if you survive
    then
        if ags ! ix == NoAgent --if cell is empty, give every neighbour a weighted probability for getting chosen to reproduce
        then do
            temp2 <- getDouble
            let Just (_, iChooseYou) = find ((>=r) . fst) cumFitAg
                neighbours = map (ags !) (moore8 ix) ++ [NoAgent] --list of the neighbours
                fitnesses = map (`fitnessAgent` env) (init neighbours)
                            ++ [0.4^P.selectionPressure]  --list of fitnesses
                cumFitnesses = scanl1 (+) fitnesses --cumulative list of fitnesses
                cumFitAg = zip cumFitnesses neighbours --list of (cumfit, agent) pairs
                r = temp2 * sum fitnesses

            iMutU <- mutAg iChooseYou
            if    iMutU /= iChooseYou
            then return $ devAg iMutU
            else return iMutU
        else return $ ags!ix
    else return NoAgent -- if you die
