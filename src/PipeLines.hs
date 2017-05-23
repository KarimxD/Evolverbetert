module PipeLines (
    main
)

    where
import qualified Data.Map as Map
import Types
import World (groupGeneTfbs, reduceToGenes, reduceToTfbss, getTfbs, hammDistChrom)
import Data.Maybe (fromMaybe, mapMaybe)
import Parsing (myShow, myRead)
-- import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.List (isPrefixOf)

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
            c <- getContents
            let line = last $ lines c
                ag = read $ last $ lewords ';' line
            putStrLn $ lineageToString ag

        _ -> putStrLn "y u no put action"
--
-- -- | Takes a function applies it on sublist not containing first n elements
-- -- skip :: Int -> ([a]->[a]) -> [a] -> [a]
-- -- skip n f = (\(a,b) -> a ++ f b) . splitAt n
--
-- | Takes a function applies it on sublist not containing first n elements
skiplines :: Int -> (SplittedLine -> SplittedLine) -> String -> String
skiplines n f = unlines . (\(a,b) -> a ++ map f2 b) . splitAt n . lines
    where f2 = leunwords ';' . f . lewords ';'
-- skiplines n f t = undefined --T.unlines $ take n ls ++ unsplinter $ drop n (map f splittedls)
--     where ls = T.lines t
--           splittedls = map splinter ls
--
--
--
--
type Line = String
type SplittedLine = [String]
--
-- -- type Lines = [T.Text]
-- -- type SplittedLines = [[T.Text]]
--
-- lastTwoToAvgIndegree :: SplittedLine -> SplittedLine
-- lastTwoToAvgIndegree = undefined
--
-- unsplinter :: SplittedLine -> Line
-- unsplinter = T.intercalate (T.pack ";")
--
-- splinter :: Line -> SplittedLine
-- splinter = T.split (==';')

lineageToString :: Agent -> String
lineageToString = timeHammToStr . timeChromToTimeHamm . lineageToTimeChrom

timeHammToStr :: [(Time, Int, Int)] -> String
timeHammToStr xs = (++)
    "time;hammdist0;hammdist1\n" $
    unlines $ reverse $ map (\(t, h0, h1) -> show t ++ ";" ++ show h0 ++ ";" ++ show h1) xs

timeChromToTimeHamm :: [(Time, Chromosome)] -> [(Time, Int, Int)]
timeChromToTimeHamm = map (\(t, c) -> (t,hammDistChrom 0 c, hammDistChrom 1 c))

lineageToTimeChrom :: Agent -> [(Time, Chromosome)]
lineageToTimeChrom NoAgent = [(0,[])]
lineageToTimeChrom ag = (t, concat.genome$ag) : lineageToTimeChrom par
    where (par, t) = parent ag

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
