module Evolverbetert (main) where
import           Misc                   (maybeCh, moore8)
import           Mutations              (mutAg)
import           MyGraphics             (showWorld)
import           MyRandom
import qualified Parameters             as P
import           World

import qualified Data.ByteString.Char8  as B (hPutStrLn)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.String            (fromString)
import           Graphics.UI.GLUT       hiding (Help, initialize, mainLoop)
import           System.Console.GetOpt
import           System.Environment     (getArgs)
import           System.IO              (Handle, IOMode (..), hClose, hFlush,
                                         openFile, stdout)

import           Control.Monad          (when)
import           Data.Fixed             (mod')
import           Data.Function          (on)
import           Data.Maybe             (fromMaybe)

import           Data.Array.IArray      (array, assocs, elems, (!))
import           Data.List              (find, maximumBy)
import           Data.List.Split        (splitEvery)
import qualified Data.Map               as Map

import qualified Control.Monad.Parallel as Par (mapM)

initialize :: [Flag] -> IO (IORef World, Handle)
initialize flags = do
    let WorldSeed sWorldSeed = fromMaybe (WorldSeed "420") (find isWorldSeed flags)
        AgentSeed sAgentSeed = fromMaybe (AgentSeed "420") (find isAgentSeed flags)
        OutputFile outputFile = fromMaybe (OutputFile "") (find isOutputFile flags)
        worldSeed = read sWorldSeed
        agentSeed = read sAgentSeed

    when (Help `elem` flags) $ ioError $ userError ('\n': usageInfo header options)

    let initialAgent = evalRand randomAgent (pureMT agentSeed)
        initialWorld = World startAgents 0
            where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent

    w <- newIORef initialWorld
    setMyStdGen $ pureMT worldSeed



    let getHandle = case find isOutputFile flags of
            Nothing             -> return stdout
            Just (OutputFile o) -> openFile o ReadWriteMode
    h <- getHandle
    B.hPutStrLn h $ fromString $ "Initial Agent is: " ++ show initialAgent
    return (w, h)
        where header = "Usage: Evolverbetert [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    (flags, strings) <- compilerOpts args
    (worldRef, h) <- initialize flags


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
    B.hPutStrLn h $ fromString "Hello, World!"
    mainLoop worldRef h 0
    B.hPutStrLn h $ fromString "Goodbye World!"
    hClose h

mainLoop :: IORef World -> Handle -> P.Time -> IO ()
mainLoop worldRef h t | t == P.maxTime = return ()
mainLoop worldRef h t = do
    w <- readIORef worldRef

    when (P.outputTime t) $
            B.hPutStrLn h (fromString $ outputString w t) >> hFlush h

    std <- getMyStdGen
    let (w',std') = runRand (newWorld w) std
    setMyStdGen std'

    writeIORef worldRef w'

    when P.display $    mainLoopEvent >> postRedisplay Nothing

    mainLoop worldRef h (t+1)

-- | It works like a clock
chEnv :: Env -> Rand Env
chEnv e = do
    r <- getRange (1, max 1 P.nrEnv-1)
    return $ (e + r) `mod'` P.nrEnv

-- | Very ugly deepseq
-- henk :: World -> IO ()
-- henk w = do
--     h <- newIORef $ Agent [[]] Map.empty
--     writeIORef h $ maximumBy (compare `on` (`fitnessAgent` env w)) $ elems (agents w)
--     readIORef h
--     return ()
-- {-# NOINLINE henk #-}

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
    ++ " " ++ show env -- current environment
    ++ " " ++ show (hammDistAg bestAgent env) -- Hamming distance of best agent
    ++ " " ++ show (length bestChrom) -- The length of the genome of best
    ++ " " ++ show bestChrom
    where
        bestAgent = maximumBy (compare `on` (`fitnessAgent` env)) $ elems ags
        bestChrom = head . genome $ bestAgent
        maxFitness (World ags env) = maximum $ map (`fitnessAgent` env) (elems ags)
        minHammDist (World ags env) = minimum $ map (`hammDistAg` env) (elems ags)

fileOutput :: World -> FilePath -> P.Time -> IO ()
fileOutput    w file t = appendFile file (outputString w t ++ "\n")
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

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]        (NoArg Help)                   "display this help info"
    , Option ['w'] ["world-seed"]  (ReqArg WorldSeed "INT")       "give the seed for the world RNG (default: 420)"
    , Option ['a'] ["agent-seed"]  (ReqArg AgentSeed "INT")       "give the seed for the first agent RNG (default: 420)"
    , Option ['o'] ["output-file"] (ReqArg OutputFile "FILEPATH") "output file"
    , Option ['g'] ["graphics"]    (NoArg Graphics)               "display CA in a window (Not yet working! Change the parameter file)"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: Evolverbetert [OPTION...] files..."

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
isGraphics    Graphics      = True; isGraphics _   = False
