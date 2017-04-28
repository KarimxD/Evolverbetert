-----------------------------------------------------------------------------
-- |
-- Module      :  Evolverbetert
-- Copyright   :  (c) Karim Hajji 2018
-- License     :  see LICENSE
--
-- Maintainer  :  k.hajji@students.uu.nl
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The program
--
-----------------------------------------------------------------------------
module Evolverbetert where
import           Misc                   (maybeCh, moore8)
import           Mutations              (mutAg)
import           MyGraphics             (showWorld)
import           MyRandom
import qualified Parameters             as P
import           World
import Types
import Parsing

import qualified Data.ByteString.Char8  as B (hPutStrLn)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.String            (fromString)
import           Graphics.UI.GLUT       hiding (Help, initialize, mainLoop)
import           System.Console.GetOpt
import           System.Environment     (getArgs, getEnv)
import           System.IO              (Handle, IOMode (..), hClose, hFlush,
                                         openFile, stdout)
import System.Directory


import           Control.Monad          (when, forM, forM_)
import           Data.Fixed             (mod')
import           Data.Function          (on)
import           Data.Maybe             (fromMaybe, isJust, isNothing, fromJust)

import           Data.Array.IArray      (array, assocs, elems, (!))
import           Data.List              (find, maximumBy)
import           Data.List.Split        (splitEvery, splitOn)
import qualified Data.Map               as Map

import qualified Control.Monad.Parallel as Par (mapM)

data Options = Options
    { optHelp        :: Bool
    , optWorldSeed   :: Int
    , optAgentSeed   :: Int
    , optOutput      :: Maybe FilePath
    , optVOutput     :: Bool
    , optConsole     :: Bool
    } deriving Show

defaultOptions = Options
    { optHelp        = False
    , optWorldSeed   = 420
    , optAgentSeed   = 420
    , optOutput      = Nothing
    , optVOutput     = False
    , optConsole     = False
    }

-- | Uses a list of flags to initialize a IORef to a world and a handle for
-- output.
-- Generates an initial agent, populates the world wit it
-- Sets the PRNG
initialize :: Options -> IO (IORef World, [Handle])
initialize opts = do
    when (optHelp opts) $ helpError []
    when (isNothing (optOutput opts) && not (optConsole opts))
        $ helpError ["\ny u no want output?!\n"]

    let initialAgent = evalRand randomAgent $ pureMT $ optAgentSeed opts
        initialWorld = World startAgents 0
            where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent

    w <- newIORef initialWorld
    setMyStdGen $ pureMT $ optWorldSeed opts

    userName <- getEnv "USER"
    let outputDir =  "/linuxhome/tmp/" ++ userName ++ "/"
    createDirectoryIfMissing False outputDir
    hs   <- case optOutput opts of
                 Just o  -> do file <- openFile (outputDir++o) ReadWriteMode
                               return [file]
                 Nothing -> return []
    hs'  <- if   optVOutput opts
            then do file <- openFile (outputDir++"v"++fromJust (optOutput opts)) ReadWriteMode
                    return [file]
            else return []
    hs'' <- if   optConsole opts
            then return [stdout]
            else return []


    return (w, hs ++ hs' ++ hs'')
    --
    -- let WorldSeed sWorldSeed = fromMaybe (WorldSeed "420") (find isWorldSeed flags)
    --     AgentSeed sAgentSeed = fromMaybe (AgentSeed "420") (find isAgentSeed flags)
    --     OutputFile outputFile = fromMaybe (OutputFile "") (find isOutputFolder flags)
    --     worldSeed = read sWorldSeed
    --     agentSeed = read sAgentSeed
    --
    -- when (Help `elem` flags) $ ioError $ userError ('\n': usageInfo header options)
    --
    -- let initialAgent = evalRand randomAgent (pureMT agentSeed)
    --     initialWorld = World startAgents 0
    --         where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent
    --
    -- w <- newIORef initialWorld
    -- setMyStdGen $ pureMT worldSeed
    --
    -- hs   <- if Console `elem` flags
    --         then return [stdout]
    --         else return []
    --
    -- hs'  <- if outputFile /= ""
    --         then do file <- openFile outputFile ReadWriteMode
    --                 return [file]
    --         else return []
    --
    -- hs'' <- if vOutputFile /= ""
    --         then do file <- openFile vOutputFile ReadWriteMode
    --                 return [file]
    --         else return []
    --
    -- let handles = hs ++ hs' ++ hs''
    --
    -- when (length handles < 1) $ error "no output specified"
    --
    -- when (isJust $ find ( (==outputFile) . show ) handles)
    --     $ return () --B.hPutStrLn h $ fromString $ "Initial Agent is: " ++ myShow initialAgent
    -- return (w, [h])
    --     where header = "Usage: Evolverbetert [OPTION...]"


-- | Maybe initializes graphical display dependent on 'P.display' in "Parameters"
-- calls 'initialize'
-- starts 'mainLoop'
main :: IO ()
main = do
    args <- getArgs
    (flags, strings) <- compilerOpts args
    (worldRef, hs) <- initialize flags


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
    B.hPutStrLn (head hs) $ fromString "Hello, World!"
    mainLoop worldRef hs 0
    B.hPutStrLn (head hs) $ fromString "Goodbye World!"
    forM_ hs hClose

-- | Recursive function that reads the IORef of the world, changes it with
-- 'newWorld' and writes to world back.
-- Also writes output if 'P.outputTime'
-- Might display graphical output using mainLoopEvent from "MyGraphics"
mainLoop :: IORef World -> [Handle] -> P.Time -> IO ()
mainLoop worldRef hs t | t == P.maxTime = return ()
mainLoop worldRef hs t = do
    w <- readIORef worldRef

    when (P.outputTime t) $ forM_ hs $
        \h -> B.hPutStrLn h (fromString $ outputString w t) >> hFlush h


    std <- getMyStdGen
    let (w',std') = runRand (newWorld w) std
    setMyStdGen std'

    writeIORef worldRef w'

    when P.display $    mainLoopEvent >> postRedisplay Nothing

    mainLoop worldRef hs (t+1)

-- | Changes the Environment dependent on 'P.nrEnv' It works like a clock
chEnv :: Env -> Rand Env
chEnv e = do
    r <- getRange (1, max 1 P.nrEnv-1)
    return $ (e + r) `mod'` P.nrEnv

-- | Changes the environment dependent on 'P.envSwitchProb' with 'chEnv'
-- Makes new agents with 'newAssoc'
newWorld :: World -> Rand World
newWorld w = do
    e' <- maybeCh e chEnv P.envSwitchProb

    newAssocs <- mapM (newAssoc w) oldAssocs -- makes new association list
    let ags' = array P.worldBounds newAssocs
        w' = World {agents = ags', env = e'}
    return w'
        where oldAssocs = assocs $ agents w
              e = env w

-- | The string of data that outputs every 'P.outputStep'
outputString :: World -> P.Time -> String
outputString w@(World ags env) t =
    show t
    ++ " " ++ show env -- current environment
    ++ " " ++ show (hammDistAg env bestAgent) -- Hamming distance of best agent
    ++ " " ++ show (length bestChrom) -- The length of the genome of best
    ++ " " ++ myShow bestChrom
    where
        bestAgent = maximumBy (compare `on` fitnessAgent env) $ elems ags
        bestChrom = head . genome $ bestAgent
        maxFitness (World ags env) = maximum $ map (fitnessAgent env) (elems ags)
        minHammDist (World ags env) = minimum $ map (hammDistAg env) (elems ags)


-- | uses 'reproduceAgent' to make a new coordinate-Agent association
newAssoc :: World -> ((Int, Int), Agent) -> Rand ((Int, Int), Agent)
newAssoc w (ix, ag) = do
    ag' <- reproduceAgent w ix
    return (ix, ag')

-- | The NSF
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
                fitnesses = map (fitnessAgent env) (init neighbours)
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




options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h','?']     ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Display this help info"
    , Option ['o']     ["output"]
        (ReqArg (\ d opts -> opts { optOutput = Just d }) "DIR")
        "Direcory for output"
    , Option ['v']     ["verbose"]
        (NoArg (\opts -> opts { optVOutput = True }))
        "Create a file to sometimes output whole world"
    , Option ['c']     []
        (NoArg (\opts -> opts { optConsole = True }))
        "Write output to console"
    , Option ['w']     ["world-seed"]
        (ReqArg (\s opts -> opts {optWorldSeed = read s}) "INT")
        "The seed used by the world"
    , Option ['a']     ["agent-seed"]
        (ReqArg (\s opts -> opts {optAgentSeed = read s}) "INT")
        "The seed used for the initial agent generation"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> helpError errs

helpError :: [String] -> IO a
helpError errs = ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

-- | Just a standard agent that can be used
-- should not be used though, as the tfbs weights have strange weights and such
genome0, genome1, genome2 :: Genome
genome2 = [map myRead $ splitOn "," "18:1,15:-2,G1:-1:0,0:-1,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-1,15:1,G15:-1:0,G7:-2:0,T,T,7:1,6:1,G14:-1:0,15:-1,10:1,G19:-1:0,10:-1,3:-1,17:1,G11:-2:0,G6:-1:0,T,18:-1,3:-1,G9:-2:0,G3:-2:0,13:-1,8:1,G2:-1:0,11:-1,7:1,G16:-1:0,T,13:1,6:1,1:-1,G18:-1:0,G17:-1:0,6:1,2:1,G0:-2:0,17:1,6:-1,G8:-1:0,8:1,16:-1,G4:-2:0,G13:-1:1"]
genome1 = [map myRead $ splitOn "," "14:-1,9:-1,G17:0,16:-1,G12:0,G19:-1,6:-1,4:1,G16:0,G18:0,17:-1,9:-1,8:-1,G0:0,G14:0,9:-1,4:-1,G10:0,9:-1,2:-1,G6:-1,3:-1,G5:-1,7:-1,G1:-1,19:-1,G7:-1,17:1,4:-1,G2:-1,G9:-1,G11:-1,G4:1,17:-1,11:-1,17:1,G8:1,7:1,G3:1,10:-1,G13:1,10:-1,3:-1,G15:1"]
genome0 = [map myRead $ splitOn "," "18:6,15:-2,G1:-1:0,0:-4,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-2,15:3,G15:-1:0,G7:-3:0,T,T,7:4,6:2,G14:-1:0,15:-6,10:5,G19:-1:0,10:-2,3:-6,17:2,G11:-2:0,G6:-1:0,T,18:-5,3:-1,G9:-3:0,G3:-3:0,13:-5,8:4,G2:-1:0,11:-5,7:4,G16:-1:0,T,13:3,6:5,1:-6,G18:-1:0,G17:-1:0,6:0,2:5,G0:-3:0,17:4,6:-1,G8:-1:0,8:5,16:-4,G4:-3:0,G13:-1:1"]

agent0 :: Agent
agent0 = devAg $ Agent genome0 defaultGst
