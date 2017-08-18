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
import           Misc                  (maybeCh, moore8)
import           Mutations             (mutAg)
import           MyGraphics            (showWorld)
import           MyRandom
import qualified Parameters            as P
import           Parsing
import           Types
import           World

import qualified Data.ByteString.Char8 as B (hPutStrLn)
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.String           (fromString)
import           Data.Time.Clock
import           Graphics.UI.GLUT      hiding (Help, initialize, mainLoop)
import           System.Console.GetOpt
import           System.Directory
import           System.Environment    (getArgs, getEnv)
import           System.IO             (Handle, IOMode (..), hFlush, openFile,
                                        stdout)
import           System.Process        (callCommand)


import           Control.Monad         (forM_, unless, when)
import           Data.Fixed            (mod')
import           Data.Function         (on)
import           Data.Maybe            (fromJust, isJust)

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

    userName <- getEnv "USER"
    UTCTime date _ <- getCurrentTime
    let outputDir = "/linuxhome/tmp/" ++ userName ++ "/Evolverbetert/" ++ show date ++ "_" ++ fromJust (optOutput opts) ++ "/"-- takeWhile (/= '.') (show time) ++ "/"
        -- opts = opts {optOutput = Just outputDir}
    when (isJust (optOutput opts) || optVOutput opts) $ do
        createDirectory outputDir
        putStrLn $ "outputDir=" ++ show outputDir
        callCommand $ "cp -r ./src/ " ++ outputDir ++ "src/"
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
    forM_ [hOutput handles''', hConsole handles'''] $ \m -> case m of
        Just h -> do B.hPutStrLn h $ fromString
                        $  "world-seed="   ++ show (optWorldSeed opts)
                        ++ "; agent-seed=" ++ show (optAgentSeed opts)
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


    -- All GLUT related stuff
    when (optGraphics opts) $ do
        _ <- getArgsAndInitialize
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
        _ <- createWindow "Evolverbetert v1"
        matrixMode $= Projection
        loadIdentity
        ortho2D 0 (fromIntegral w / fromIntegral pixelsPerUnit)
                0 (fromIntegral h / fromIntegral pixelsPerUnit)

        displayCallback $= showWorld worldRef
        actionOnWindowClose $= MainLoopReturns
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

    when (P.outputTime t) $ forM_ [hOutput hs, hConsole hs] $ \m -> case m of
        Just h -> B.hPutStrLn h (fromString $ outputString w t False) >> hFlush h
        _      -> return ()

    when (P.vOutputTime t) $ case hVOutput hs of
        Just h -> B.hPutStrLn h (fromString $ show t ++ " ; " ++ show (cleanTrace w))
        _      -> return ()

    when (P.lineageTime t && optLineage opts) $ do
        let b = maximumBy (compare `on` fitness (env w)) (agents w)
        writeFile (cwd ++ "lineage") (agentToLineageFile b)

        -- Just h -> let b = maximumBy (compare `on` fitnessAgent (env w)) (agents w)
        --            in undefined
                --    in B.hPutStrLn h (fromString $ show t ++ ";" ++ show (env w) ++ ";" ++ show b)
        -- _      -> return ()

    std <- getMyStdGen
    let (w',std') = runRand (newWorld t w) std
    setMyStdGen std'

    writeIORef worldRef w'

    when (optGraphics opts) $ mainLoopEvent >> postRedisplay Nothing

    mainLoop worldRef opts cwd hs (t+1)

-- | Changes the Environment dependent on 'P.nrEnv' It works like a clock
chEnv :: Env -> Rand Env
chEnv e = do
    r <- getRange (1, max 1 P.nrEnv-1)
    return $ (e + r) `mod'` P.nrEnv

-- | Changes the environment dependent on 'P.envSwitchProb' with 'chEnv'
-- Makes new agents with 'newAssoc'
newWorld :: Time -> World -> Rand World
newWorld t w = do
    e' <- maybeCh e chEnv P.envSwitchProb

    newAssocs <- mapM (newAssoc t w) oldAssocs -- makes new association list
    let ags' = array P.worldBounds newAssocs
        w' = World {agents = ags', env = e'}
    return w'
        where oldAssocs = assocs $ agents w
              e = env w


-- | The string of data that outputs every 'P.outputStep'
-- Needs an input world, time
-- if True then prints a header for the outputTable
outputString :: World -> Time -> Bool -> String
outputString (World ags e) t r =
    intercalate ";"
        [f _t, f _e, f _minHammDist, f _minOtherHammDist, f _maxHammDist, f _avgHammDist, f _lenBestChrom, f' _bestChrom, f' _bestOtherChrom]

    -- ++ myShow bestChrom
    where
        f :: Show a => (a, String) -> String -- | either show the thing or the discription
        f = if r then snd else show . fst
        f' :: MyShow a => (a, String) -> String
        f'= if r then snd else myShow . fst

        _t = (t, "time")
        _e = (e, "env")

        _bestAgent         = (maximumBy (compare `on` fitness e) els,        "bestAgent")
        _bestOtherAgent    = (maximumBy (compare `on` fitness otherenv) els, "bestOtherAgent")

        _worstAgent        = (minimumBy (compare `on` fitness e) els,        "worstAgent")
        _worstOtherAgent   = (minimumBy (compare `on` fitness otherenv) els, "worstOtherAgent")

        _bestChrom         = (concat . genome $ fst _bestAgent,       "bestChrom")
        _bestOtherChrom    = (concat . genome $ fst _bestOtherAgent,  "bestOtherChrom")
        _worstChrom        = (concat . genome $ fst _worstAgent,      "worstChrom")
        _worstOtherChrom   = (concat . genome $ fst _worstOtherAgent, "worstOtherChrom")

        _lenBestChrom      = (length $ fst _bestChrom,      "lenBestChrom")
        _lenBestOtherChrom = (length $ fst _bestOtherChrom, "lenBestOtherChrom")
        _lenWorstChrom     = (length $ fst _worstChrom,     "lenWorstChrom")
        _lenWorstOtherCrom = (length $ fst _bestOtherChrom, "lenWorstOtherChrom")

        _maxFitness        = (fitness e $ fst _bestAgent,  "maxFitness")
        _minFitness        = (fitness e $ fst _worstAgent, "minFitness")

        _minHammDist       = (hammDist e $ fst _bestAgent,              "minHammDist")
        _minOtherHammDist  = (hammDist otherenv $ fst _bestOtherAgent,  "minOtherHammDist")
        _maxHammDist       = (hammDist e $ fst _worstAgent,             "maxHammDist")
        _maxOtherHammDist  = (hammDist otherenv $ fst _worstOtherAgent, "maxOtherHammDist")

        _avgHammDist       = (average $ map (hammDist e) els,        "avgHammDist")
        _avgOtherHammDist  = (average $ map (hammDist otherenv) els, "avgHammDist")

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
            let Just (_, iChooseYou) = find ((>=r) . fst) cumFitAg
                neighbours = map (ags !) (moore8 ix) ++ [NoAgent] --list of the neighbours
                fitnesses = map (fitness e) (init neighbours)
                            ++ [0.4^P.selectionPressure]  --list of fitnesses
                cumFitnesses = scanl1 (+) fitnesses --cumulative list of fitnesses
                cumFitAg = zip cumFitnesses neighbours --list of (cumfit, agent) pairs
                r = temp2 * sum fitnesses

            iMutU <- mutAg iChooseYou
            if iMutU == NoAgent
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
        "The seed used by the world"
    , Option ['a']     ["agent-seed"]
        (ReqArg (\s opts -> opts {optAgentSeed = read s}) "INT")
        "The seed used for the initial agent generation"
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

-- | Just a standard agent that can be used
-- should not be used though, as the tfbs weights have strange weights and such
-- genome0, genome1, genome2 :: Genome
-- genome2 = [map myRead $ splitOn "," "18:1,15:-2,G1:-1:0,0:-1,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-1,15:1,G15:-1:0,G7:-2:0,T,T,7:1,6:1,G14:-1:0,15:-1,10:1,G19:-1:0,10:-1,3:-1,17:1,G11:-2:0,G6:-1:0,T,18:-1,3:-1,G9:-2:0,G3:-2:0,13:-1,8:1,G2:-1:0,11:-1,7:1,G16:-1:0,T,13:1,6:1,1:-1,G18:-1:0,G17:-1:0,6:1,2:1,G0:-2:0,17:1,6:-1,G8:-1:0,8:1,16:-1,G4:-2:0,G13:-1:1"]
-- genome1 = [map myRead $ splitOn "," "14:-1,9:-1,G17:0,16:-1,G12:0,G19:-1,6:-1,4:1,G16:0,G18:0,17:-1,9:-1,8:-1,G0:0,G14:0,9:-1,4:-1,G10:0,9:-1,2:-1,G6:-1,3:-1,G5:-1,7:-1,G1:-1,19:-1,G7:-1,17:1,4:-1,G2:-1,G9:-1,G11:-1,G4:1,17:-1,11:-1,17:1,G8:1,7:1,G3:1,10:-1,G13:1,10:-1,3:-1,G15:1"]
-- genome0 = [map myRead $ splitOn "," "18:6,15:-2,G1:-1:0,0:-4,G5:-1:0,G12:-1:0,T,G10:-2:0,5:-2,15:3,G15:-1:0,G7:-3:0,T,T,7:4,6:2,G14:-1:0,15:-6,10:5,G19:-1:0,10:-2,3:-6,17:2,G11:-2:0,G6:-1:0,T,18:-5,3:-1,G9:-3:0,G3:-3:0,13:-5,8:4,G2:-1:0,11:-5,7:4,G16:-1:0,T,13:3,6:5,1:-6,G18:-1:0,G17:-1:0,6:0,2:5,G0:-3:0,17:4,6:-1,G8:-1:0,8:5,16:-4,G4:-3:0,G13:-1:1"]

-- agent0 :: Agent
-- agent0 = devAg $ Agent genome0 defaultGst (NoAgent, 0)

cleanTrace :: World -> World
cleanTrace w =
    w { agents = amap cleanParent $ agents w }
    where cleanParent NoAgent = NoAgent
          cleanParent a       = a {parent = NoAgent}

agent42 :: Agent
agent42 = read "Agent {genome = [[CTfbs (Tfbs {tfbsID = ID 18, wt = Weight 1}),CGene (Gene {geneID = ID 14, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 21, wt = Weight (-1)}),CGene (Gene {geneID = ID 19, thres = Thres (-1), genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1)}),CGene (Gene {geneID = ID 20, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight (-1)}),CGene (Gene {geneID = ID 3, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 20, wt = Weight (-1)}),CGene (Gene {geneID = ID 4, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 21, wt = Weight 1}),CGene (Gene {geneID = ID 16, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight 1}),CGene (Gene {geneID = ID 24, thres = Thres 0, genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight 1}),CGene (Gene {geneID = ID 13, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 24, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 22, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight (-1)}),CGene (Gene {geneID = ID 22, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight 1}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 13, wt = Weight 1}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 4, wt = Weight (-1)}),CGene (Gene {geneID = ID 6, thres = Thres 0, genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 0, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 19, wt = Weight 1}),CGene (Gene {geneID = ID 10, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1)}),CGene (Gene {geneID = ID 5, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 8, wt = Weight 1}),CGene (Gene {geneID = ID 9, thres = Thres 2, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 20, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 18, wt = Weight (-1)}),CGene (Gene {geneID = ID 1, thres = Thres 2, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 1, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 23, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1}),CTfbs (Tfbs {tfbsID = ID 22, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 3, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 11, wt = Weight (-1)}),CGene (Gene {geneID = ID 0, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight 1}),CGene (Gene {geneID = ID 23, thres = Thres 2, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 7, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight 1}),CGene (Gene {geneID = ID 17, thres = Thres 0, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 6, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 2, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight 1}),CGene (Gene {geneID = ID 2, thres = Thres 1, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 15, wt = Weight (-1)}),CTfbs (Tfbs {tfbsID = ID 10, wt = Weight 1}),CGene (Gene {geneID = ID 21, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 14, wt = Weight 1}),CGene (Gene {geneID = ID 18, thres = Thres 0, genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 24, wt = Weight 1}),CTfbs (Tfbs {tfbsID = ID 5, wt = Weight (-1)}),CGene (Gene {geneID = ID 8, thres = Thres 2, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 17, wt = Weight 1}),CGene (Gene {geneID = ID 7, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 16, wt = Weight 1}),CGene (Gene {geneID = ID 12, thres = Thres 2, genSt = GS False}),CTfbs (Tfbs {tfbsID = ID 9, wt = Weight 1}),CGene (Gene {geneID = ID 15, thres = Thres (-1), genSt = GS True}),CTfbs (Tfbs {tfbsID = ID 12, wt = Weight 1}),CTfbs (Tfbs {tfbsID = ID 23, wt = Weight 1}),CGene (Gene {geneID = ID 11, thres = Thres (-1), genSt = GS True})]], geneStateTable = fromList [(ID 0,GS False),(ID 1,GS False),(ID 2,GS False),(ID 3,GS False),(ID 4,GS False),(ID 5,GS True),(ID 6,GS True),(ID 7,GS True),(ID 8,GS False),(ID 9,GS False),(ID 10,GS True),(ID 11,GS True),(ID 12,GS False),(ID 13,GS True),(ID 14,GS True),(ID 15,GS True),(ID 16,GS True),(ID 17,GS False),(ID 18,GS True),(ID 19,GS False),(ID 20,GS False),(ID 21,GS True),(ID 22,GS False),(ID 23,GS False),(ID 24,GS True)], bornTime = 0, bornEnv = 0, parent = NoAgent, diff = []}"
