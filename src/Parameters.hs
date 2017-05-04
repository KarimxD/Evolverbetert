module Parameters where
import           GHC.Word
import Types
type Time = Int
type Prob = Double




-- defaultWorldSeed = 420 :: Int
-- defaultInitialAgentSeed = 420 :: Int
-- defaultOutputFile = "/home/karim/Data/output.txt" :: String
display = False :: Bool

-- * World Parameters

width  = 50 :: Int
height = 50 :: Int
worldBounds = ((0::Int, 0::Int), (width-1, height-1))
worldCoods = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
nrEnv = 2 :: Int

-- * Time Parameters
vOutputStep = 10000 :: Time
vOutputTime = (0 ==) . flip rem vOutputStep
outputStep = 50 :: Time
outputTime t = t `mod` outputStep == 0
dumpStep = 1000
dumpTime t = t `mod` dumpStep == 0
maxTime = -1 --round 1e6 -- 500000
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
nrGeneTypes' = (\(ID a) -> a) nrGeneTypes
nrGeneTypes = ID $ nrHouseHold + nrOverlap + nrSpecific -- usually 20
nrHouseHold = 8 :: Int; nrOverlap = 0 :: Int; nrSpecific = 12 :: Int
minThres = -2 :: Int; maxThres = 2 :: Int

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
