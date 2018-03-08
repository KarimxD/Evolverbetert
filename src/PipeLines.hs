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
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Parsing
-- import World (groupGeneTfbs)
import           Misc --(verticalHistogram)
-- import qualified Data.Text as T
import           Data.List
import qualified Data.Map as Map
import qualified Analysis as Anal
import           Landscapes
import Safe

import System.Directory (getCurrentDirectory, makeRelativeToCurrentDirectory)
import           System.Environment (getArgs)

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import TextShow
-- import Data.String

-- import Data.Traversable
import Data.Foldable

import Control.Parallel.Strategies

import Data.Time.Clock
import System.IO


main :: IO ()
main = do
    args <- getArgs
    -- let _     :args'    = args
    --     arg1  :args''   = args'
    --     arg2       :args'''  = args''
    --     arg3      :_        = args'''
        -- n = read n' :: SampleSize

    currentdir <- makeRelativeToCurrentDirectory =<< getCurrentDirectory
    let arg1:t1 = args
        arg2:t2 = t1
        arg3:_ = t2

        params'' = parseParameters t1
        params'  = params'' { chromConverter = headDef (error "no metric specified") $ mapMaybe (`parseConverter` params') args }
        params   = params' { resetGeneStatesOnBirth = "reset1" `isPrefixOf` currentdir
                           }

    -- let lineageHandle = withFile "lineage" ReadMode
    case arg1 of
        "help" -> TIO.putStrLn "Check the source file... run inside data folder (with files output and lineage)"
        "ontime" -> TIO.putStrLn =<< withFile "lineage" ReadMode (onTime arg2 (chromConverter params))
        "convert" -> do
            c <- TIO.getContents
            let action = arg2
            makeFile c (lineConverter action params) ("lineagedir/" ++ action)
        "convert2" -> do
            c <- TIO.getContents
            let action = fromJustDef (error "parser not found") $ parseConverter2 arg2 params :: Chromosome -> Chromosome -> T.Text
                timeandchromosomes = map (\(t,_,chrom,_) -> (t,chrom)) $ tParseLineageFile c :: [(Time,Chromosome)]
                (times, chromosomes) = unzip timeandchromosomes
                times' = map showt times
                chromPairs = combineListToTuples chromosomes :: [(Chromosome, Chromosome)]
                output' = withStrategy (parBuffer 100 rseq) $ map (uncurry action) chromPairs
                output = T.unlines $ map tUnWords $ zipWith (\x y -> [x,y]) times' output'

            TIO.writeFile ("lineagedir/" ++ arg2) output
        "numrem" ->
            interact $
                  show . zipWith (analyzeChrom . numRemaining) (map (\n->params{sampleSize=n})[10,100,1000,10000,100000,1000000])
                . repeat . getLastChrom
        "rnet" -> do
            c <- getContents
            let chrom = getLastChrom c
                (edges, nodes) = Anal.chromosomeToRNet chrom
                edgefile = arg2
                nodefile = arg3
            writeFile edgefile edges
            writeFile nodefile nodes
        "lineage" -> do
            c <- getContents
            let line = last $ lines c
                ag = read $ last $ lewords ';' line
            putStrLn . agentToLineageFile $ ag
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
        "genestates"  -> do
            c <- TIO.getContents
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
        "splitlineage" ->
            let defaultparams = params {sampleSize = 1000, seed = 420, numberOfUpdates = 20}
                def s = withFile "lineage" ReadMode (\h ->
                  do
                    c <- TIO.hGetContents h
                    time1 <- getCurrentTime
                    putStr $ "Step: " ++ s ++ ". Time: "
                    hFlush stdout
                    makeFile c (lineConverter s defaultparams) ("lineagedir/" ++ s)
                    time2 <- getCurrentTime
                    print $ diffUTCTime time2 time1
                  )
            in  mapM_ def [ "env", "hammdist", "mutations"
                          , "statenum"
                          , "attrnum", "listattr", "listpointattr", "pointattrnum"
                          , "targets", "startinggst"
                          , "chromosome", "chromosome_p", "genlength"
                          , "gst", "gst_p"
                          , "copynumbers"
                          , "developmenttime"
                          ]
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
type LineConverter = LineageLine -> T.Text

parseParameters :: [String] -> Parameters
parseParameters = flip pp Parameters{}
    where
        pp :: [String] -> Parameters -> Parameters
        pp [] p = p
        pp ("samplesize":n:rest) p = pp rest $ p {sampleSize = read n}
        pp ("updates":n:rest)    p = pp rest $ p {numberOfUpdates = read n}
        pp ("seed":n:rest)       p = pp rest $ p {seed = read n}
        pp ("reset":n:rest)      p = pp rest $ p {resetGeneStatesOnBirth = numToBool n}
        pp (_:xs) p = pp xs p
        numToBool "0" = False; numToBool _ = True

parseConverter :: String -> Parameters -> Maybe (Chromosome -> T.Text)
parseConverter "statenum"      _ = Just $ showt . analyzeChrom nrOfStates
parseConverter "attrnum"       p = Just $ showt . analyzeChrom (attrNum p)
parseConverter "listattr"      p = Just $ showt . analyzeChrom (listAttr p)
parseConverter "listpointattr" p = Just $ showt . analyzeChrom (listPointAttr p)
parseConverter "pointattrnum"  p = Just $ showt . analyzeChrom (pointAttrNum p)
parseConverter "targets"       _ = Just $ showt . analyzeChrom targets
parseConverter "startinggst"   _ = Just $ showt . analyzeChrom startGSTAttr
parseConverter "attractorbasin" p= Just $ showt . fromJustDef (-1) . analyzeChrom (theAttractorBasin p)
parseConverter "developmenttime"p= Just $ showt . analyzeChrom (developmentTime p)
-- parseConverter "rem"           p = Just $ showt . analyzeChrom (remaining p)
parseConverter "avgindegree"   _ = Just $ showt . avgIndegreeChrom
parseConverter "genlength"     _ = Just $ showt . length
parseConverter "chromosome"    _ = Just   showt
parseConverter "chromosome_p"  _ = Just $ T.pack . myShow
parseConverter "gst"           _ = Just $ showt . toGST
parseConverter "gst_p"         _ = Just $ T.pack . myShow . toGST
parseConverter "statenet"      p = Just $ T.pack . analyzeChrom (stateNetwork p)
parseConverter "dot"           _ = Just $ T.pack . Anal.chromosomeToDot
parseConverter "copynumbers"   _ = Just $ tUnWords . map tMyShow . Map.elems . analyzeChrom fullGST
parseConverter "allstatenet"   _ = Just $ T.pack . analyzeChrom allStateNetwork -- | Don't use on big chromosomes (have >1000000 states)
parseConverter _               _ = Nothing --error $ "undefined converter: Pipelines.parseConverter. was given : " ++ n

parseConverter2 :: String -> Parameters -> Maybe (Chromosome -> Chromosome -> T.Text)
parseConverter2 "divergence"   _ = Just $ (showt .) . divergencePointOfTrajectory
parseConverter2 "divergencehd" _ = Just $ (showt .) . divergencePointOfTrajectoryOnOff
parseConverter2 "distancebetween" _ = Just$ (showt .) . distanceAfterMutation
parseConverter2 _            _   = Nothing

lineConverter :: String -> Parameters -> LineConverter
lineConverter name params =
    case name of
        "env"      -> \(t,e,_,_) -> tUnWords [showt t, showt   e]
        "hammdist"  -> \(t,e,c,_) -> tUnWords [showt t, showt $ hammDist e c]
        "mutations" -> \(t,_,_,m) -> tUnWords [showt t, showt   m]
        _           -> \(t,_,c,_) -> tUnWords [showt t, func    c]
    where func = fromJustDef (error ("\nParser with name {"++name++"} not found: Pipelines.lineConverter"))
                            $ parseConverter name params

makeFile :: T.Text -- | Contents
         -> LineConverter
         -> FilePath -- | to write to
         -> IO ()
makeFile c f outputFile = do
    -- let output = T.unlines $ map f $ tParseLineageFile c
    let output = T.unlines $ withStrategy (parBuffer 100 rseq) $ map f $ tParseLineageFile c
    TIO.writeFile outputFile output

onTime :: String -> (Chromosome -> a) -> Handle -> IO a
onTime s f h = f <$> getTimeChrom s h

getTimeChrom :: String -> Handle -> IO Chromosome
getTimeChrom "last" h = getLastChrom <$> hGetContents h
getTimeChrom t      h = do
    wds <- map (C.split ';') . C.lines <$> C.hGetContents h
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

lastToAvgIndegree :: [String] -> [String]
lastToAvgIndegree = map (
    unwords .
    (\ws -> init ws ++ [show . avgIndegree . myRead . last $ ws] ) . words)

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
