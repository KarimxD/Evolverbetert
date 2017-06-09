module PipeLines (
    main
)

    where
import qualified Data.Map as Map
import Types
-- import Misc
import World (groupGeneTfbs, reduceToGenes, reduceToTfbss, getTfbs, hammDistChrom, fitnessAgent)
import Data.Maybe (fromMaybe, mapMaybe)
import Parsing (myShow, myRead, agentToLineageFile)
-- import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.List (isPrefixOf, group)

import           System.Environment     (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let action:args' = args
    case action of
        "dot" -> do
            c <- getContents
            let time:_ = args'
                line = head $ filter (time `isPrefixOf`) $ lines c
                chrom = head $ mapMaybe readMaybe (lewords ';' line) :: Chromosome
            putStrLn $ genomeToDot [chrom]
        "net" -> do
            c <- getContents
            putStrLn $ unlines $ lastToAvgIndegree (lines c)
        "twonet" -> do
            c <- getContents
            putStrLn . skiplines 2 lastTwoToAvgIndegree $ c
            -- putStrLn . T.unpack . T.unlines . skip 2 lastTwoToAvgIndegree . T.lines $ T.pack c
        "onlynet" -> do
            c <- getContents
            putStrLn $ networkproperties (timeGenome c)
        "lineage" -> do
            -- let output:_ = args'
            c <- getContents
            -- o <- readFile output
            let line = last $ lines c
                ag = read $ last $ lewords ';' line
                -- lineage = reverse $ agentToLineage ag
                -- timeEnvs = outputToTimeEnv o

            -- putStrLn $ show $ fitnessAgent 0 ag
            -- print lineage
            -- putStrLn $ unlines $ compress $ lines $ henk timeEnvs lineage
            putStrLn . agentToLineageFile $ ag

        _ -> putStrLn "y u no put action"
--
-- -- | Takes a function applies it on sublist not containing first n elements
-- -- skip :: Int -> ([a]->[a]) -> [a] -> [a]
-- -- skip n f = (\(a,b) -> a ++ f b) . splitAt n
--
compress :: Eq a => [a] -> [a]
compress = map head . group

-- | Takes a function applies it on sublist not containing first n elements
skiplines :: Int -> (SplittedLine -> SplittedLine) -> String -> String
skiplines n f = unlines . (\(a,b) -> a ++ map f2 b) . splitAt n . lines
    where f2 = leunwords ';' . f . lewords ';'

type Line = String
type SplittedLine = [String]



-- | envs -> lineage -> output
-- henk :: [(Time,Env)] -> [(Time,Chromosome)] -> String
-- henk [] _ = ""
-- henk _ [] = ""
-- henk envs@((te,e):restenvs) chroms@((tc,c):restchroms) =
--         if tc > te
--         then henk restenvs chroms
--         else show te ++ ";" ++ show e ++ ";" ++ myShow c ++ "\n" ++ henk envs restchroms

-- outputToTimeEnv :: String -> [(Time,Env)]
-- outputToTimeEnv c = map (lineToTimeEnv . lewords ';') ls
--     where ls = drop 2 $ lines c
--           lineToTimeEnv (t:e:_) = (read t, read e) :: (Time, Env)



-- roundTimes :: Int -> [(Time, a)] -> [(Time, a)]
-- roundTimes _ []         = []
-- roundTimes i ((t, a):xs) = (roundToNearest i t, a) : roundTimes i xs



-- lineageToTrail :: Agent ->

-- lineageToString :: Agent -> String
-- lineageToString = timeHammToStr . timeChromToTimeHamm . lineageToTimeChrom

-- timeHammToStr :: [(Time, Int, Int)] -> String
-- timeHammToStr xs = (++)
--     "time;hammdist0;hammdist1;agent\n" $
--     unlines $ reverse $ map (\(t, h0, h1) -> show t ++ ";" ++ show h0 ++ ";" ++ show h1) xs

-- timeChromToTimeHamm :: [(Time, Chromosome)] -> String
-- timeChromToTimeHamm xs = (++)
--     "time;hammdist0;hammdist1;chrom\n" $
--     unlines $ map (\(t,c) -> show t ++ ";" ++ chromToStr c) xs
--         where   chromToStr :: Chromosome -> String
--                 chromToStr c = show (hammDistChrom 0 c) ++ ";" ++ show (hammDistChrom 1 c) ++ ";" ++ myShow c
--
-- lineageToTimeChrom :: Agent -> [(Time, Chromosome)]
-- lineageToTimeChrom NoAgent = [(0,[])]
-- lineageToTimeChrom ag = (t, concat.genome$ag) : lineageToTimeChrom par
--     where (par, t) = parent ag

lewords                   :: Char -> String -> [String]
lewords c s               =  case dropWhile (==c) s of
                                "" -> []
                                s' -> w : lewords c s''
                                      where (w, s'') =
                                             break (==c) s'

leunwords                 :: Char -> [String] -> String
leunwords _ []              =  ""
leunwords c ws              =  foldr1 (\w s -> w ++ c:s) ws

lastTwoToAvgIndegree :: SplittedLine -> SplittedLine
lastTwoToAvgIndegree = reverse .
    (\(x:y:r) -> (show . avgIndegree $ read $ f x) : (show . avgIndegree $ read $ f y) : r)
    . reverse
        where f a = "[" ++ a ++ "]" :: String
--
-- lastTwoToAvgIndegree =
--
-- type Lines = [T.Text]
-- type Splitted = [[T.Text]]
--

lastToAvgIndegree :: [String] -> [String]
lastToAvgIndegree = map (
    unwords .
    (\ws -> init ws ++ [show . avgIndegree . myRead . last $ ws] ) . words)


-- | Displays time and avg_indegree
networkproperties :: [(Time,Genome)] -> String
networkproperties = unlines . map (\(i,g) -> unwords [show i, show (avgIndegree g)])

avgIndegree :: Genome -> Double
avgIndegree g = fromIntegral (length (reduceToTfbss g))
       / fromIntegral (length (reduceToGenes g))

-- | Chromosome has to be last, and time first
timeGenome :: String -> [(Time, Genome)]
timeGenome = map (readfstnlst . words) . drop 1 . lines
    where
        readfstnlst ws = (read (head ws), myRead (last ws))

-- | Make a dotfile from a genome (concatinates genome and calls 'chromosomeToDot')
genomeToDot :: Genome -> String
genomeToDot = chromosomeToDot . concat

-- | Make a dotfile from a chromosome
-- inhibiting edges are red, exciting blue
chromosomeToDot :: Chromosome -> String
chromosomeToDot c = "digraph geneNetwork {\n" ++
    concat (groupedToDot Map.empty (groupGeneTfbs c) counts)
    ++ "}"
    where
        counts = constructCounter (reduceToGenes [c]) Map.empty

groupedToDot :: Counter -> [[Locus]] -> Counts -> [String]
groupedToDot _ [] _ = []
groupedToDot counter (loci:locis) counts = s : groupedToDot newcounter locis counts
    where
        ([CGene g], tfbss) = splitAt 1 $ reverse loci
        c = 1 + fromMaybe (-1) (Map.lookup (iD g) counter)
        s = tfbssGeneToDot (g,c) counts (mapMaybe getTfbs tfbss)
        newcounter = Map.insert (iD g) c counter

tfbssGeneToDot :: NumberedGene -> Counts -> [Tfbs] -> String
tfbssGeneToDot g counts = concatMap (geneTfbsToDot g counts)

geneTfbsToDot :: NumberedGene -> Counts -> Tfbs -> String
geneTfbsToDot g counts t =
    style ++
    concat ( zipWith
        (\i r -> "    " ++ i ++ r ++ color ++ ";\n")
        its (repeat ("->" ++ ig)))

    where
        its = map ( (++) ("G" ++ myShow (iD t) ++ "x") . show)
                  [ 0 .. ( fromMaybe 0 (Map.lookup (iD t) counts) ) ]
        ig = "G" ++ myShow (iD (fst g)) ++ "x" ++ show (snd g)
        color = case wt t of
            (1) -> " [color=green]"
            _    -> " [color=red]"
        style = case genSt (fst g) of
            GS True -> "    " ++ ig ++ " [style = filled];\n"
            _       -> ""

type NumberedGene = (Gene, Int)
type Counter = Map.Map ID Int
type Counts = Counter

constructCounter :: [Gene] -> Counter -> Counter
constructCounter [] c = c
constructCounter (g:gs) counter = constructCounter gs newcounter
    where c = 1 + fromMaybe (-1) (Map.lookup (iD g) counter)
          newcounter = Map.insert (iD g) c counter
