module Parameters where
import GHC.Word
type Time = Int
type Prob = Double

outputFile = "/home/karim/Data/output.txt"

devTime = 20 :: Int

worldSeed = 420 :: Word64
agent0Seed = 420 :: Word64

width  = 50 :: Int
height = 50 :: Int
display = False :: Bool
outputMode = Console
outputStep = 50 :: Time

worldBounds = ((0::Int, 0::Int), (width-1, height-1))
worldCoods = [(x, y) | x <- [0..width-1], y <- [0..height-1]]

maxTime = 200 :: Int --floor 6e5 :: Int -- 500000 :: Int
-- maxTime = floor 6e5 :: Int -- 500000 :: Int

selectionPressure = 10 :: Int
deathRate = 0.3 :: Prob
envSwitchProb = 3e-3 :: Prob

nrEnv = 2 :: Int

{-
 When only 2 environments are used,
 NrOverlap and NrSpecific contribute to the same
 (namely genes expressed in A, and not in B, and vice versa)
-}
nrGeneTypes = nrHouseHold + nrOverlap + nrSpecific :: Int -- usually 20
nrHouseHold = 8 :: Int; nrOverlap = 0 :: Int; nrSpecific = 12 :: Int

minThres = -2 :: Int; maxThres = 2 :: Int

-- Mutational Parameters
pGenDel     = 3e-4 :: Prob
pGenDup     = 2e-4 :: Prob
pGenThresCh = 5e-6 :: Prob

pTfbsWtCh   = 2e-5 :: Prob
pTfbsPrefCh = 2e-5 :: Prob
pTfbsDel    = 3e-5 :: Prob
pTfbsInnov  = 1e-5 :: Prob -- scales with number of loci in genome
pTfbsDup    = 2e-5 :: Prob

data OutputMode = File | Console | FileAndConsole
