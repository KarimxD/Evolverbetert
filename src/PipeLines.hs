{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module PipeLines

    where
-- import qualified Data.Map           as Map
import           Types
import           Fitness
-- import Misc
import           Data.Maybe         (fromMaybe, mapMaybe, fromJust)
import           Parsing
-- import World (groupGeneTfbs)
import           Misc --(verticalHistogram)
-- import qualified Data.Text as T
import           Data.List
import qualified Analysis as Anal
import           Landscapes
import Safe

import           System.Environment (getArgs)

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import TextShow
-- import Data.String

-- import Data.Traversable
import Data.Foldable

import System.Directory
import Control.Parallel.Strategies


main :: IO ()
main = do
    args <- getArgs
    -- let _     :args'    = args
    --     arg1  :args''   = args'
    --     arg2       :args'''  = args''
    --     arg3      :_        = args'''
        -- n = read n' :: SampleSize
    let cwd :t1 = args
        arg1:t2 = t1
        arg2:t3 = t2
        arg3:_ = t3

        params' = parseParameters args
        params  = params' { chromConverter = headMay $ mapMaybe (`parseConverter` params') args }

    setCurrentDirectory cwd
    case arg1 of
        "ontime" -> let t = arg2
            in print =<< onTime t (fromJust $ chromConverter params)
               -- case arg3 of (fromJust $ chromConverter params)
                -- "gst"           -> putStrLn =<< (myShow <$> onTime arg2 (analyzeChrom fullGST))
                -- "statenum"      -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "attrnum"       -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "listattr"      -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "listpointattr" -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "pointattrnum"  -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "targets"       -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "startingGST"   -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "rem"           -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "statenet"      -> putStrLn =<< onTime t (fromJust $ chromConverter params)
                -- "state"         -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "chromosome"    -> print    =<< onTime t (fromJust $ chromConverter params)
                -- "dot"           -> putStrLn =<< onTime t Anal.chromosomeToDot
                -- "allstatenet"   -> interact $ analyzeChrom allStateNetwork . getLastChrom
                -- _ -> error "No valid action given: Pipelines"

        -- "makenetworks"-> do
        --     c <- T.lines <$> TIO.getContents
        --     createDirectoryIfMissing False $ cwd ++ "networks"
        --     let cwd' = cwd ++ "networks/"
        --         ts = map tRead $ T.splitOn "-" $ T.pack t :: [Time]
        --         i:j:_ = map (`findTime` c) ts :: [Int]
        --         ls = slice i j c :: [T.Text]
        --     undefined
        "numrem" ->
            interact $
                  show . zipWith (analyzeChrom . numRemaining) (map (\n->Parameters{sampleSize=n})[10,100,1000,10000,100000,1000000])
                . repeat . getLastChrom
        "rnet" -> do
            c <- getContents
            let chrom = getLastChrom c
                (edges, nodes) = Anal.chromosomeToRNet chrom
                edgefile = arg2
                nodefile = arg3
            writeFile edgefile edges
            writeFile nodefile nodes
        -- "dot" -> --do
        --     -- c <- getContents
        --     -- let chrom = getLastChrom c :: Chromosome
        --     --
        --     -- putStrLn $ Anal.chromosomeToDot chrom
        --     putStrLn =<< onTime t Anal.chromosomeToDot
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
                    \case
                           GenDup _ -> True
                           GenDel _ -> True
                           _        -> False
                    ) mutations
                ids = map iD dupdels
                printthis = verticalHistogram ids
            putStr printthis
        "attrs" -> do
            c <- C.readFile "lineage"
            let parsedls = cParseLineageFile c
                f = analyzeChrom $ listAttr params
                showAttrs :: Int -> [(Int,Int,[Int])] -> [C.ByteString]
                showAttrs t' ((a,b,[x,y]):xs) = cUnWords (map cShow [t',a,b,x,y]) : showAttrs t' xs
                showAttrs _ _ = []
                avghammdists = C.unlines $ concatMap (\(t',_,ch,_) -> showAttrs t' $ f ch) parsedls
            C.writeFile "lineagedir/attrs" avghammdists
        "states" -> do
            c <- C.readFile "lineage"
            let parsedls = cParseLineageFile c
                f = analyzeChrom stateOfChrom
                avghammdists = C.unlines $ map (\(t',_,ch,_) -> cUnWords
                    [cShow t', cShow (f ch)]) parsedls
            C.writeFile "lineagedir/states" avghammdists
        "genestates"  -> do
            c <- TIO.readFile "lineage"
            let parsedls = tParseLineageFile c
                f = T.words . tMyShow . toGST
            --     genestates = T.unlines $ map (\(t',_,ch,_) -> tUnWords
            --         (showt t' : f ch)) parsedls
            -- TIO.writeFile "lineagedir/genestates" genestates

            let genestates = map (\(t',_,ch,_) -> tUnWords
                    (showt t' : f ch) `T.append` "\n") parsedls
            forM_ genestates $ TIO.appendFile "lineagedir/genestates"
        "avghammdist" -> do
            c <- C.readFile "lineage"
            let parsedls = cParseLineageFile c
                f = analyzeChrom (avgFitness 1 [0,1] params)
                henk (x:y:_) = [cShow x, cShow y]
                henk _ = error "dont do this plz"
                avghammdists = C.unlines $ map (\(t',_,ch,_) -> cUnWords
                    (cShow t' : henk (f ch)) ) parsedls
            C.writeFile "lineagedir/avghammdists" avghammdists
        "splitlineage" -> do
            c <- TIO.getContents
            let defaultparams = params {sampleSize = 1000, seed = 420}
                def s = makeFile c (lineConverter s defaultparams) (cwd ++ "lineagedir/")

            def "envs"
            def "hammdists"
            def "genlength"
            def "mutations"
            def "attrnums"
        -- "attractornumber" -> do
        --     c <- C.readFile "lineage"
        --     let parsedls = cParseLineageFile c
        --         attrnums = C.unlines $ map (\(t,_,ch,_) -> cUnWords
        --             [cShow t, cShow (analyzeChrom (attrNum 10000) ch)])    parsedls
        --     createDirectoryIfMissing False $ cwd ++ "lineagedir"
        --     let cwd' = "lineagedir/"
        --     C.writeFile (cwd' ++ "attractornumbers") attrnums

        _ -> putStrLn "y u no put good action"



--
-- -- | Takes a function applies it on sublist not containing first n elements
-- -- skip :: Int -> ([a]->[a]) -> [a] -> [a]
-- -- skip n f = (\(a,b) -> a ++ f b) . splitAt n
--
-- compress :: Eq a => [a] -> [a]
-- compress = map head . group

type LineageLine = (Time,Env,Chromosome,[Mutation])
type LineageFile = [LineageLine]
type ConverterName = String
type LineConverter = (LineageLine -> T.Text, ConverterName)

parseParameters :: [String] -> Parameters
parseParameters = flip pp Parameters{}
    where
        pp :: [String] -> Parameters -> Parameters
        pp [] p = p
        pp ("samplesize":n:rest) p = pp rest $ p {sampleSize = read n}
        pp ("updates":n:rest)    p = pp rest $ p {numberOfUpdates = read n}
        pp ("seed":n:rest)       p = pp rest $ p {seed = read n}
        pp (_:xs) p = pp xs p

parseConverter :: String -> Parameters -> Maybe (Chromosome -> T.Text)
parseConverter "genlength" _     = Just $ showt . length
parseConverter "statenum"      _ = Just $ showt . analyzeChrom nrOfStates
parseConverter "attrnum"       p = Just $ showt . analyzeChrom (attrNum p)
parseConverter "listattr"      p = Just $ showt . analyzeChrom (listAttr p)
parseConverter "listpointattr" p = Just $ showt . analyzeChrom (listPointAttr p)
parseConverter "pointattrnum"  p = Just $ showt . analyzeChrom (pointAttrNum p)
parseConverter "targets"       _ = Just $ showt . analyzeChrom targets
parseConverter "startingGST"   _ = Just $ showt . analyzeChrom startGSTAttr
parseConverter "rem"           p = Just $ showt . analyzeChrom (remaining p)
parseConverter "statenet"      p = Just $ showt . analyzeChrom (stateNetwork p)
parseConverter "state"         _ = Just $ showt . analyzeChrom stateOfChrom
parseConverter "chromosome"    _ = Just   showt
parseConverter "dot"           _ = Just $ showt . Anal.chromosomeToDot
parseConverter "allstatenet"   _ = Just $ showt . analyzeChrom allStateNetwork
parseConverter _               _ = Nothing


lineConverter :: String -> Parameters -> LineConverter
lineConverter name params =
    (
    case name of
        "envs"      -> \(t,e,_,_) -> tUnWords [showt t, showt e]
        "hammdists" -> \(t,e,c,_) -> tUnWords [showt t, showt $ hammDist e c]
        "genlength" -> \(t,_,c,_) -> tUnWords [showt t, showt $ length c]
        "mutations" -> \(t,_,_,m) -> tUnWords [showt t, showt   m]
        "attrnums"  -> \(t,_,c,_) -> tUnWords [showt t, showt $ analyzeChrom (attrNum params) c]
        _           -> error "undefined converter: Pipelines.lineConverter"
    , name)

makeFile :: T.Text -- | Contents
         -> LineConverter
         -> FilePath -- | to write to
         -> IO ()
makeFile c (f, converterName) outputPath = do
    -- let output = T.unlines $ map f $ tParseLineageFile c
    let output = T.unlines $ withStrategy (parBuffer 100 rseq) $ map f $ tParseLineageFile c
    TIO.writeFile (outputPath ++ converterName) output

onTime :: String -> (Chromosome -> a) -> IO a
onTime s f = f <$> getTimeChrom s

getTimeChrom :: String -> IO Chromosome
getTimeChrom "last" = getLastChrom <$> getContents
getTimeChrom t      = do
    wds <- map (C.split ';') . C.lines <$> C.getContents
    let l' = find ((>= (read t :: Time))
                . cRead . head) wds
        l = fromMaybe (error "time not found") l'
        c = mapMaybe (readMaybe . C.unpack) l
    return $ head c

findTime :: Time -> [T.Text] -> Int
findTime t =
    fromMaybe (error "time not found") . findIndex (>= t)
    . map (tRead . head . T.splitOn ";")

onLast :: (Chromosome -> a) -> IO a
onLast f = f . getLastChrom <$> getContents

getLastChrom :: String -> Chromosome
getLastChrom s = head $ mapMaybe readMaybe (lewords ';' (last $ lines s))

extractChromFromLine :: String -> Maybe Chromosome
extractChromFromLine = headMay . mapMaybe readMaybe . lewords ';'

extractChromFromLine' :: T.Text -> Maybe Chromosome
extractChromFromLine' t = case map tRead $ T.splitOn ";" t of
    x:_ -> Just x
    _   -> Nothing

lineagelineToHd :: C.ByteString -> C.ByteString
lineagelineToHd = C.unlines . map
    (cUnWords .
       (\(t : e : c : _) -> [t, C.pack $ show $
             hammDist (read $ C.unpack e) (myRead $ C.unpack c :: Chromosome)])
    . cWords) . C.lines

cParseLineageFile :: C.ByteString -> LineageFile
cParseLineageFile content = parsedls
    where
        ls = C.lines content
        splittedls = map (C.split ';') ls
        parsedls = map (\(t:e:c:m:_) -> (cRead t, cRead e, cMyRead c, cRead m)) splittedls

tParseLineageFile :: T.Text -> LineageFile
tParseLineageFile content = parsedls
    where
        ls = T.lines content
        splittedls = map (T.splitOn ";") ls
        parsedls = map (\(t:e:c:m:_) -> (tRead t, tRead e, tMyRead c, tRead m)) splittedls

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
cUnWords = C.intercalate ";"

tWords :: T.Text -> [T.Text]
tWords = T.splitOn ";"

tUnWords :: [T.Text] -> T.Text
tUnWords = T.intercalate ";"

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
