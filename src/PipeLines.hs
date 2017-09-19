module PipeLines

    where
-- import qualified Data.Map           as Map
import           Types
import           Fitness
-- import Misc
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Parsing            (agentToLineageFile, myRead, myShow, cRead, cMyRead, cMyShow, cShow, readMaybe)
-- import World (groupGeneTfbs)
import           Misc (verticalHistogram)
-- import qualified Data.Text as T
import           Data.List          (find)
import qualified Analysis as Anal
import qualified Landscapes as Land

import           System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C


import System.Directory

main :: IO ()
main = do
    args <- getArgs
    let cwd:args' = args
    let action:args'' = args'
    setCurrentDirectory cwd
    case action of
        "statenum" ->
            interact (show . length . Land.allGSTofChrom . getLastChrom)
        "attrnum" ->
            interact (show . Land.attrNum' . getLastChrom)
        "numrem" ->
            interact (show . Land.numRemaining . getLastChrom)
        "rnet" -> do
            c <- getContents
            let chrom = getLastChrom c
                (edges, nodes) = Anal.chromosomeToRNet chrom
                edgefile = head args''
                nodefile = head $ tail args''
            writeFile edgefile edges
            writeFile nodefile nodes
        "dot" -> do
            c <- getContents
            let chrom = getLastChrom c :: Chromosome
            putStrLn $ Anal.chromosomeToDot chrom
        "net" ->
            interact $ unlines . lastToAvgIndegree . lines
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
        "lineagetohd" -> C.interact lineagelineToHd

        "dupdels" -> do --feed it the lineagefile
            c <- getContents
            let parsedls = parseLineageFile c
                mutations = concatMap (\(_,_,_,muts) -> muts) parsedls
                dupdels = filter (
                    \x -> case x of
                           GenDup _ -> True
                           GenDel _ -> True
                           _        -> False
                    ) mutations
                ids = map iD dupdels
                printthis = verticalHistogram ids
            putStr printthis
        "splitlineage" -> do
            c <- C.readFile "lineage"
                -- [t,e,chrom,[muts]]
            let parsedls = cParseLineageFile c
                envs      = C.unlines $ map (\(t,e,_,_)  -> cUnWords
                    [cShow t, cShow e])               parsedls
                hammdists = C.unlines $ map (\(t,e,ch,_) -> cUnWords
                    [cShow t, cShow $ hammDist e ch]) parsedls
                genlength = C.unlines $ map (\(t,_,ch,_) -> cUnWords
                    [cShow t, cShow $ length ch])     parsedls
                mutations = C.unlines $ map (\(t,_,_,ms) -> cUnWords
                    [cShow t, cShow ms])              parsedls

            createDirectoryIfMissing False $ cwd ++ "lineagedir"
            let cwd' = "lineagedir/"
            C.writeFile (cwd' ++ "envs"     ) envs
            C.writeFile (cwd' ++ "hammdists") hammdists
            C.writeFile (cwd' ++ "genlength") genlength
            C.writeFile (cwd' ++ "mutations") mutations
        "attractornumber" -> do
            c <- C.readFile "lineage"
            let parsedls = cParseLineageFile c
                attrnums = C.unlines $ map (\(t,_,ch,_) -> cUnWords
                    [cShow t, cShow (Land.attrNum ch)])    parsedls
            createDirectoryIfMissing False $ cwd ++ "lineagedir"
            let cwd' = "lineagedir/"
            C.writeFile (cwd' ++ "attractornumbers") attrnums

        _ -> putStrLn "y u no put good action"

--
-- -- | Takes a function applies it on sublist not containing first n elements
-- -- skip :: Int -> ([a]->[a]) -> [a] -> [a]
-- -- skip n f = (\(a,b) -> a ++ f b) . splitAt n
--
-- compress :: Eq a => [a] -> [a]
-- compress = map head . group

type LineageFile = [(Time,Env,Chromosome,[Mutation])]

getLastChrom :: String -> Chromosome
getLastChrom s = head $ mapMaybe readMaybe (lewords ';' (last $ lines s))

lineagelineToHd :: C.ByteString -> C.ByteString
lineagelineToHd = C.unlines . map
    (cUnWords .
       (\(t : e : c : _) -> [t, C.pack $ show $
             hammDist (read $ C.unpack e) (myRead $ C.unpack c :: Chromosome)])
    . cWords) . C.lines

cParseLineageFile :: C.ByteString -> [(Time,Env,Chromosome,[Mutation])]
cParseLineageFile content = parsedls
    where
        ls = C.lines content
        splittedls = map (C.split ';') ls
        parsedls = map (\(t:e:c:m:_) -> (cRead t, cRead e, cMyRead c, cRead m)) splittedls

parseLineageFile :: String -> [(Time,Env,Chromosome,[Mutation])]
parseLineageFile content = parsedls
    where
        ls = lines content
        splittedls = map (lewords ';') ls
        parsedls = map (\(t:e:c:m:_) -> (read t, read e, myRead c, read m)) splittedls

-- | Takes a function applies it on sublist not containing first n elements
skiplines :: Int -> (SplittedLine -> SplittedLine) -> String -> String
skiplines n f = unlines . (\(a,b) -> a ++ map f2 b) . splitAt n . lines
    where f2 = leunwords ';' . f . lewords ';'

type Line = String
type SplittedLine = [String]

loadLineage :: String -> [(Time, Env, Chromosome)]
loadLineage = map loadLine . lines
    where
        -- loadLine :: Line -> (Time, Env, Chromosome)
        loadLine = (\(a:b:c:_) -> (read a, read b, myRead c)) . lewords ';'

cWords :: C.ByteString -> [C.ByteString]
cWords = C.split ';'

cUnWords :: [C.ByteString] -> C.ByteString
cUnWords = C.intercalate (C.pack ";")

lewords                   :: Char -> String -> [String]
lewords c s               =  case dropWhile (==c) s of
                                "" -> []
                                s' -> w : lewords c s''
                                      where (w, s'') =
                                             break (==c) s'

leunwords                 :: Char -> [String] -> String
leunwords _ [] =  ""
leunwords c ws =  foldr1 (\w s -> w ++ c:s) ws

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
avgIndegree = avgIndegreeChrom . concat

avgIndegreeChrom :: Chromosome -> Double
avgIndegreeChrom g = fromIntegral (nrTfbss g) /  fromIntegral (nrGenes g)
-- avgIndegree g = fromIntegral (length (reduceGenomeToTfbss g))
--        / fromIntegral (length (reduceGenomeToGenes g))

-- | Chromosome has to be last, and time first
timeGenome :: String -> [(Time, Genome)]
timeGenome = map (readfstnlst . words) . drop 1 . lines
    where
        readfstnlst ws = (read (head ws), myRead (last ws))

nrGenes, nrTfbss :: Chromosome -> Int
nrGenes = length . filter isGene
nrTfbss = length . filter isGene

copyNumberGene :: ID -> Chromosome -> Int
copyNumberGene gt = length . filter isgenetype
    where isgenetype (CGene g) = iD g == gt
          isgenetype _         = False


fromTime :: Time -> [(Time, Env, Chromosome)] -> (Time, Env, Chromosome)
fromTime t0 list = fromMaybe (last list)
                           $ find (\(t, _, _) -> t0 >= t) list


-- -- | Make a dotfile from a genome (concatinates genome and calls 'chromosomeToDot')
-- genomeToDot :: Genome -> String
-- genomeToDot = chromosomeToDot . concat
--
-- -- | Make a dotfile from a chromosome
-- -- inhibiting edges are red, exciting blue
-- chromosomeToDot :: Chromosome -> String
-- chromosomeToDot c =
--     "// " ++ myShow c ++ "\n" ++
--     "digraph geneNetwork {\n" ++
--     concat (groupedToDot Map.empty (groupGeneTfbs c) counts)
--     ++ "}"
--     where
--         counts = constructCounter (reduceChromToGenes c) Map.empty
--
-- groupedToDot :: Counter -> [[Locus]] -> Counts -> [String]
-- groupedToDot _ [] _ = []
-- groupedToDot counter (loci:locis) counts = s : groupedToDot newcounter locis counts
--     where
--         ([CGene g], tfbss) = splitAt 1 $ reverse loci
--         c = 1 + fromMaybe (-1) (Map.lookup (iD g) counter)
--         s = tfbssGeneToDot (g,c) counts (mapMaybe getTfbs tfbss)
--         newcounter = Map.insert (iD g) c counter
--
-- tfbssGeneToDot :: NumberedGene -> Counts -> [Tfbs] -> String
-- tfbssGeneToDot g counts = concatMap (geneTfbsToDot g counts)
--
-- geneTfbsToDot :: NumberedGene -> Counts -> Tfbs -> String
-- geneTfbsToDot g counts t =
--     style ++
--     concat ( zipWith
--         (\i r -> "    " ++ i ++ r ++ color ++ ";\n")
--         its (repeat ("->" ++ ig)))
--
--     where
--         its = map ( (++) ("G" ++ myShow (iD t) ++ "x") . show)
--                   [ 0 .. ( fromMaybe 0 (Map.lookup (iD t) counts) ) ]
--         ig = "G" ++ myShow (iD (fst g)) ++ "x" ++ show (snd g)
--         color = case wt t of
--             (1) -> " [color=green]"
--             _   -> " [color=red]"
--         style = case genSt (fst g) of
--             GS 0       -> ""
--             GS _       -> "    " ++ ig ++ " [style = filled];\n"
--
-- type NumberedGene = (Gene, Int)
-- type Counter = Map.Map ID Int
-- type Counts = Counter
--
-- constructCounter :: [Gene] -> Counter -> Counter
-- constructCounter [] c = c
-- constructCounter (g:gs) counter = constructCounter gs newcounter
--     where c = 1 + fromMaybe (-1) (Map.lookup (iD g) counter)
--           newcounter = Map.insert (iD g) c counter
