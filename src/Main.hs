-----------------------------------------------------------------------------
-- |
-- Module      :  Main
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
module Main where
import           Misc                  (moore8)
import           Mutations             (mutAg)
import           MyGraphics            (initializeWorld, callShowWorld)
import           MyRandom
import qualified Parameters            as P
import           Parsing
import           Types
import           World
import           Fitness

import qualified Data.Map              as Map
import qualified Data.ByteString.Char8 as B (hPutStrLn)
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.String           (fromString)
import           Data.Time.Clock
-- import           Graphics.UI.GLUT      hiding (Help, initialize, mainLoop)
import           System.Console.GetOpt
import           System.Directory
import           System.Environment    (getArgs, getEnv)
import           System.IO             (Handle, IOMode (..), hFlush, openFile,
                                        stdout)
import           System.Process        (callCommand)


import           Control.Monad         (forM_, unless, when)
import           Data.Fixed            (mod')
import           Data.Function         (on)
import           Data.Maybe            (fromJust, isJust, fromMaybe)

import           Data.Array.IArray     (amap, array, assocs, elems, (!))
import           Data.List             (find, genericLength, intercalate,
                                        maximumBy, minimumBy)
-- import           Data.List.Split       (splitOn)

type CWD = String
data Handles = Handles {
      hOutput  :: Maybe Handle
    , hVOutput :: Maybe Handle
    , hConsole :: Maybe Handle
    , hLineage :: Maybe Handle
}
stdHandles :: Handles
stdHandles = Handles Nothing Nothing Nothing Nothing

-- | Uses a list of flags to initialize a IORef to a world and a handle for
-- output.
-- Generates an initial agent, populates the world wit it
-- Sets the PRNG
initialize :: Options -> IO (IORef World, CWD, Handles)
initialize opts = do
    when (optHelp opts) $ helpError []
    unless (isJust (optOutput opts) || optConsole opts || optVOutput opts || optGraphics opts)
        $ helpError ["\ny u no want output?!\n"]

    let initialAgent = evalRand randomAgent $ pureMT $ optAgentSeed opts
        initialWorld = World startAgents 0
            where startAgents = array P.worldBounds $ zip P.worldCoods $ repeat initialAgent

    w <- newIORef initialWorld

    setMyStdGen $ pureMT $ optWorldSeed opts
    setMyEnvGen $ pureMT $ optEnvSeed opts

    userName <- getEnv "USER"
    UTCTime date _ <- getCurrentTime
    -- let outputDir = "/linuxhome/tmp/" ++ userName ++ "/Evolverbetert/"
    --              ++ show date ++ "_" ++ fromJust (optOutput opts) ++ "/"-- takeWhile (/= '.') (show time) ++ "/"
    --     opts = opts {optOutput = Just outputDir}
    -- when (isJust (optOutput opts) || optVOutput opts) $ do
    --     createDirectory outputDir
    --     putStrLn $ "outputDir=" ++ show outputDir
    --     callCommand $ "cp -r ./src/ " ++ outputDir ++ "src/"
    --     putStrLn "copied source directories"

    let outputDir = "/linuxhome/tmp/" ++ userName ++ "/Evolverbetert/" ++ fromJust (optOutput opts) ++ "_" ++ show date ++ "/"-- takeWhile (/= '.') (show time) ++ "/"
        -- opts = opts {optOutput = Just outputDir}
    when (isJust (optOutput opts) || optVOutput opts) $ do
        createDirectory outputDir
        putStrLn $ "outputDir=" ++ show outputDir
        callCommand $ "cp -r ~/Evolverbetert/src/ " ++ outputDir ++ "src/"
        putStrLn "copied source directories"

    let handles = stdHandles

    handles' <- if isJust $ optOutput opts
                then do file <- openFile (outputDir++"output") ReadWriteMode
                        return handles {hOutput = Just file}
                else return handles {hOutput = Nothing}

    handles'' <- if optVOutput opts
                 then do file <- openFile (outputDir++"voutput") ReadWriteMode
                         return handles' {hVOutput = Just file}
                 else return handles' {hVOutput = Nothing}

    handles''' <- if optConsole opts
                  then return handles'' {hConsole = Just stdout}
                  else return handles'' {hConsole = Nothing}

    -- handles''''<- if optLineage opts
    --               then do file <- openFile (outputDir++"lineage") ReadWriteMode
    --                       return handles''' {hLineage= Just file}
    --               else return handles''' {hLineage = Nothing}

    -- Display some info about the initialization and the header for the output table
    forM_ [hOutput handles''', hConsole handles'''] $ \case
        Just h -> do B.hPutStrLn h $ fromString
                        $  "world-seed="   ++ show (optWorldSeed opts)
                        ++ "; agent-seed=" ++ show (optAgentSeed opts)
                        ++ "; env-seed=" ++ show (optEnvSeed opts)
                        ++ "; initialAgent = " ++ myShow initialAgent
                     B.hPutStrLn h $ fromString $ outputString initialWorld 0 True
        _      -> return ()
    return (w, outputDir, handles''')


-- | Maybe initializes graphical display dependent on 'P.display' in "Parameters"
-- calls 'initialize'
-- starts 'mainLoop'
main :: IO ()
main = do

    args <- getArgs
    (opts, _) <- compilerOpts args
    (worldRef, cwd, hs) <- initialize opts

    -- print $ iterate updateAgent agent42 !! 1000


    -- All GLUT related stuff
    when (optGraphics opts) (initializeWorld worldRef)
    -- Where the magic happens... Recursive function that ends on P.maxTime
    -- B.hPutStrLn (fromMaybe stdout $ hOutput hs) $ fromString "Hello, World!"
    mainLoop worldRef opts cwd hs 0
    -- B.hPutStrLn (fromMaybe stdout $ hOutput hs) $ fromString "Goodbye World!"

-- | Recursive function that reads the IORef of the world, changes it with
-- 'newWorld' and writes to world back.
-- Also writes output if 'P.outputTime'
-- Might display graphical output using mainLoopEvent from "MyGraphics"
mainLoop :: IORef World -> Options -> CWD -> Handles -> Time -> IO ()
mainLoop _ _ _ _ t | t == P.maxTime = return ()
mainLoop worldRef opts cwd hs t = do
    w <- readIORef worldRef

    when (P.outputTime t) $ forM_ [hOutput hs, hConsole hs] $ \case
        Just h -> B.hPutStrLn h (fromString $ outputString w t False) >> hFlush h
        _      -> return ()

    when (P.vOutputTime t) $ case hVOutput hs of
        Just h -> B.hPutStrLn h (fromString $ show t ++ " ; " ++ show (orphanizeWorld w))
        _      -> return ()

    when (P.lineageTime t && optLineage opts) $ do
        let b = maximumBy (compare `on` fitness (env w)) (agents w)
        writeFile (cwd ++ "lineage") (agentToLineageFile b)

        -- Just h -> let b = maximumBy (compare `on` fitnessAgent (env w)) (agents w)
        --            in undefined
                --    in B.hPutStrLn h (fromString $ show t ++ ";" ++ show (env w) ++ ";" ++ show b)
        -- _      -> return ()

    envGen <- getMyEnvGen
    let (newEnv, envGen') = runRand (maybeCh (env w) chEnv P.envSwitchProb) envGen
        -- w' = w {env = newEnv}
    setMyEnvGen envGen'


    std <- getMyStdGen
    let (w',std') = runRand (newWorld t newEnv w) std
    setMyStdGen std'

    writeIORef worldRef w'

    when (optGraphics opts) callShowWorld

    mainLoop worldRef opts cwd hs (t+1)

-- | Changes the Environment dependent on 'P.nrEnv' It works like a clock
chEnv :: Env -> Rand Env
chEnv e = do
    r <- getRange (1, max 1 P.nrEnv-1)
    return $ (e + r) `mod'` P.nrEnv

-- | Changes the environment dependent on 'P.envSwitchProb' with 'chEnv'
-- Makes new agents with 'newAssoc'
newWorld :: Time -> Env -> World -> Rand World
newWorld t actualNewEnv !w = do
    _ <- getDouble -- performance *2 ?

    newAssocs <- mapM (newAssoc t w) oldAssocs -- makes new association list
    let ags' = array P.worldBounds newAssocs
        w' = World {agents = ags', env = actualNewEnv}
    return w'
        where oldAssocs = assocs $ agents w


-- | The string of data that outputs every 'P.outputStep'
-- Needs an input world, time
-- if True then prints a header for the outputTable
outputString :: World -> Time -> Bool -> String
outputString (World ags e) t r =
    intercalate ";"
        [f _t, f _e, f _minHammDist, f _minOtherHammDist, f _maxHammDist, f _avgHammDist, f _lenBestChrom, f' _bestChrom, f' _bestOtherChrom, myShow $ geneStateTable $ fst _bestAgent]

    -- ++ myShow bestChrom
    where
        f :: Show a => (a, String) -> String -- | either show the thing or the discription
        f = if r then snd else show . fst
        f' :: MyShow a => (a, String) -> String
        f'= if r then snd else myShow . fst

        _t = (t, "time")
        _e = (e, "env")

        _bestAgent         = (maximumBy (compare `on` fitness e) els,        "bestAgent" :: String)
        _bestOtherAgent    = (maximumBy (compare `on` fitness otherenv) els, "bestOtherAgent" :: String)

        _worstAgent        = (minimumBy (compare `on` fitness e) els,        "worstAgent" :: String)
        _worstOtherAgent   = (minimumBy (compare `on` fitness otherenv) els, "worstOtherAgent" :: String)

        _bestChrom         = (concat . genome $ fst _bestAgent,       "bestChrom" :: String)
        _bestOtherChrom    = (concat . genome $ fst _bestOtherAgent,  "bestOtherChrom" :: String)
        _worstChrom        = (concat . genome $ fst _worstAgent,      "worstChrom" :: String)
        _worstOtherChrom   = (concat . genome $ fst _worstOtherAgent, "worstOtherChrom" :: String)

        _lenBestChrom      = (length $ fst _bestChrom,      "lenBestChrom" :: String)
        _lenBestOtherChrom = (length $ fst _bestOtherChrom, "lenBestOtherChrom" :: String)
        _lenWorstChrom     = (length $ fst _worstChrom,     "lenWorstChrom" :: String)
        _lenWorstOtherCrom = (length $ fst _bestOtherChrom, "lenWorstOtherChrom" :: String)

        _maxFitness        = (fitness e $ fst _bestAgent,  "maxFitness" :: String)
        _minFitness        = (fitness e $ fst _worstAgent, "minFitness" :: String)

        _minHammDist       = (hammDist e $ fst _bestAgent,              "minHammDist" :: String)
        _minOtherHammDist  = (hammDist otherenv $ fst _bestOtherAgent,  "minOtherHammDist" :: String)
        _maxHammDist       = (hammDist e $ fst _worstAgent,             "maxHammDist" :: String)
        _maxOtherHammDist  = (hammDist otherenv $ fst _worstOtherAgent, "maxOtherHammDist" :: String)

        _avgHammDist       = (average $ map (hammDist e) els,        "avgHammDist" :: String)
        _avgOtherHammDist  = (average $ map (hammDist otherenv) els, "avgHammDist" :: String)

        els = filter living $ elems ags
        otherenv = 1 + (-1)*e

        average :: (Real a) => [a] -> Double
        average xs = realToFrac (sum xs) / genericLength xs


-- | uses 'reproduceAgent' to make a new coordinate-Agent association
newAssoc :: Time -> World -> ((Int, Int), Agent) -> Rand ((Int, Int), Agent)
newAssoc t w (ix, _) = do
    ag' <- reproduceAgent t w ix
    return (ix, ag')

-- | The NSF
reproduceAgent :: Time -> World -> (Int, Int) -> Rand Agent
reproduceAgent t (World ags e) ix = do
    temp1 <- getDouble
    if temp1 > P.deathRate --if you survive
    then
        if ags ! ix == NoAgent --if cell is empty, give every neighbour a weighted probability for getting chosen to reproduce
        then do
            temp2 <- getDouble
            let (_, iChooseYou) = fromMaybe (error "field empty") $ find ((>=r) . fst) cumFitAg
                neighbours = map (ags !) (moore8 ix) ++ [NoAgent] --list of the neighbours
                fitnesses = map (fitness e) (init neighbours)
                            ++ [0.4^P.selectionPressure]  --list of fitnesses
                cumFitnesses = scanl1 (+) fitnesses --cumulative list of fitnesses
                cumFitAg = zip cumFitnesses neighbours --list of (cumfit, agent) pairs
                r = temp2 * sum fitnesses

            iMutU <- mutAg iChooseYou
            if (iMutU == NoAgent) || P.nrGeneTypes' > Map.size (geneStateTable iMutU)
            then return NoAgent
            else --return $ devAg $ iMutU {parent = (iChooseYou, t)}
                if   null $ diff iMutU
                then return         iMutU {diff = diff iChooseYou}
                else return $ devAg iMutU { parent = iChooseYou, bornTime = t, bornEnv = e }        else return $ ags!ix
    else return NoAgent -- if you die


data Options = Options
    { optHelp      :: Bool
    , optWorldSeed :: Int
    , optAgentSeed :: Int
    , optEnvSeed :: Int
    , optOutput    :: Maybe FilePath
    , optVOutput   :: Bool
    , optLineage   :: Bool
    , optConsole   :: Bool
    , optGraphics  :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optHelp        = False
    , optWorldSeed   = 420
    , optAgentSeed   = 420
    , optEnvSeed     = 420
    , optOutput      = Nothing
    , optVOutput     = False
    , optLineage     = False
    , optConsole     = False
    , optGraphics    = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h','?']     ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Display this help info"
    , Option ['o']     ["output"]
        (ReqArg (\s opts -> opts { optOutput = Just s }) "name")
        "Direcory for output"
    , Option ['v']     ["verbose"]
        (NoArg (\opts -> opts { optVOutput = True }))
        "Create a file to sometimes output whole world -- only works in combination with -o"
    , Option ['l']     ["lineage-trace"]
        (NoArg (\opts -> opts { optLineage = True }))
        "Output lineage trace in file -- only works in combination with -o"
    , Option ['c']     []
        (NoArg (\opts -> opts { optConsole = True }))
        "Write output to console"
    , Option ['w']     ["world-seed"]
        (ReqArg (\s opts -> opts {optWorldSeed = read s}) "INT")
        "The seed used by the world. Default: 420"
    , Option ['a']     ["agent-seed"]
        (ReqArg (\s opts -> opts {optAgentSeed = read s}) "INT")
        "The seed used for the initial agent generation. Default: 420"
    , Option ['e']     ["environment-seed"]
        (ReqArg (\s opts -> opts {optEnvSeed = read s}) "INT")
        "The seed used for switching the environment. Default: 420"
    , Option ['g']     ["graphics"]
        (NoArg (\opts -> opts { optGraphics = True }))
        "Display CA in graphical window"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> helpError errs

helpError :: [String] -> IO a
helpError errs = ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

orphanizeWorld :: World -> World
orphanizeWorld w =
    w { agents = amap orphanize $ agents w }
    where orphanize NoAgent = NoAgent
          orphanize a       = a {parent = NoAgent}

-- agent42 :: Agent
-- agent42 = read "Agent {genome = [[CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 2, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 8, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 0, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 7, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 16, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 5, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 10, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 13, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 14, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 18, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 9, thres = Thres (-1), genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 17, thres = Thres 2, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 19, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 3, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1, tfbsSt = GS 1}),CGene (Gene {geneID = ID 11, thres = Thres 0, genSt = GS 1}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1), tfbsSt = GS 1}),CGene (Gene {geneID = ID 12, thres = Thres 0, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 15, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 1, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight (-1), tfbsSt = GS 0}),CGene (Gene {geneID = ID 4, thres = Thres 1, genSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1, tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1), tfbsSt = GS 0}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight 1, tfbsSt = GS 0}),CGene (Gene {geneID = ID 6, thres = Thres 0, genSt = GS 0})]], geneStateTable = fromList [(ID 0,GS 0),(ID 1,GS 0),(ID 2,GS 0),(ID 3,GS 0),(ID 4,GS 0),(ID 5,GS 1),(ID 6,GS 0),(ID 7,GS 0),(ID 8,GS 1),(ID 9,GS 1),(ID 10,GS 0),(ID 11,GS 1),(ID 12,GS 0),(ID 13,GS 0),(ID 14,GS 1),(ID 15,GS 0),(ID 16,GS 0),(ID 17,GS 0),(ID 18,GS 1),(ID 19,GS 0)], bornTime = 0, bornEnv = 0, parent = NoAgent, diff = []}"
