{-# OPTIONS_GHC -w #-}
{-# LANGUAGE NumDecimals #-}

module Parameters
    where
import Types
import Data.Map as Map


-- * Mode Parameters
resetGeneStatesOnBirth = False :: Bool
dosiseffect = True :: Bool
nrNoEffect = 0 :: Int

-- defaultWorldSeed = 420 :: Int
-- defaultInitialAgentSeed = 420 :: Int
-- defaultOutputFile = "/home/karim/Data/output.txt" :: String
display = False :: Bool

-- * World Parameters
width  = 150 :: Int
height = 50 :: Int
worldBounds = ((0::Int, 0::Int), (width-1, height-1))
worldCoods = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
nrEnv = 2 :: Int

-- * Time Parameters
vOutputStep = 50000 :: Time
outputStep  = 50    :: Time
dumpStep    = 1000  :: Time
lineageStep = 200 :: Time
dumpTime    = (0 ==) . (`mod` dumpStep)    :: Time -> Bool
outputTime  = (0 ==) . (`mod` outputStep)  :: Time -> Bool
vOutputTime = (0 ==) . (`mod` vOutputStep) :: Time -> Bool
lineageTime = (0 ==) . (`mod` lineageStep) :: Time -> Bool

maxTime = 200 -- 1e6 -- 500000
     :: Int

-- * Fitness Parameters
devTime = 20 :: Int -- # of steps agent gets to find attractor of network
selectionPressure = 10 :: Int
deathRate = 0.3 :: Prob


-- * Genome and network properties

{- |
 When only 2 environments are used,
 NrOverlap and NrSpecific contribute to the same
 (namely genes expressed in A, and not in B, and vice versa)
-}
nrGeneTypes' = (\(ID a) -> a) nrGeneTypes :: Int
nrGeneTypes = ID $ nrHouseHold + nrOverlap + nrSpecific + nrNoEffect -- usually 20
nrFitEffect = ID $ nrHouseHold + nrOverlap + nrSpecific
nrFitEffect' = (\(ID a) -> a) nrFitEffect :: Int
nrHouseHold = 8 :: Int; nrOverlap = 0 :: Int; nrSpecific = 12 :: Int;
-- nrHouseHold = 4 :: Int; nrOverlap = 3 :: Int; nrSpecific = 5 :: Int; nrNoEffect = 0 :: Int
minThres = -1 :: Int; maxThres = 2 :: Int -- -1 and 0 turn off, 1 and 2 turn on

-- * Probabilities for mutation and environmental change
envSwitchProb = 3e-4 :: Prob

pGenDel     = 3e-4 :: Prob
pGenDup     = 2e-4 :: Prob
pGenThresCh = 5e-6 :: Prob

pTfbsWtCh   = 2e-5 :: Prob
pTfbsPrefCh = 2e-5 :: Prob

pTfbsDel    = 3e-5 :: Prob
pTfbsInnov  = 1e-5 :: Prob -- scales with number of loci in genome FIX: scale with genes
pTfbsDup    = 2e-5 :: Prob


{- | startingGST lays in between the attractors of targetExpression.
    For instance nrEnv = 4 and nrHouseHold = 4, nrOverlap = 3, nrSpecific = 5
    Env\Gene    0   1   2   3   4   5   6   7   8   9   10  11
    0           1   1   1   1   0   1   1   1   0   0   0   1
    1           1   1   1   1   1   0   1   0   1   0   0   0
    2           1   1   1   1   1   1   0   0   0   1   0   0
    3           1   1   1   1   0   1   1   0   0   0   1   0
    start       1   1   0   0   1   1   0   1   1   1   0   0
-}
startingGST :: GST
startingGST = Map.fromList $ zip [0..] $ fhsh hh ++ fhsh ov ++ fhsh sp ++ fhsh ne
    where fhsh x -- firsthalfsecondhalf
            | even x    = replicate (x `div` 2    ) 1 ++ replicate (x `div` 2) 0
            | otherwise = replicate (x `div` 2 + 1) 1 ++ replicate (x `div` 2) 0
          hh = nrHouseHold; ov = nrOverlap; sp = nrSpecific; ne = nrNoEffect
