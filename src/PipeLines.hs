module PipeLines (
      genomeToDot
    , chromosomeToDot
)

    where
import qualified Data.Map as Map
import Types
import World (groupGeneTfbs, reduceToGenes, getTfbs)
import Data.Maybe (fromMaybe, mapMaybe)
import Parsing (myShow, myRead)

import           System.Environment     (getArgs, getEnv)
import System.IO (openFile, IOMode (..), hGetLine)

main :: IO ()
main = do
    args <- getArgs
    let action:file:_ = args
    h <- openFile file ReadMode
    case action of
        "dot" -> do
            l <- hGetLine h
            putStrLn $ genomeToDot $ myRead l
        _ -> putStrLn "y u no put action"

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
groupedToDot counter [] counts = []
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
    (concat $ zipWith
        (\i r -> "    " ++ i ++ r ++ color t ++ ";\n")
        its (repeat ("->" ++ ig)))

    where
        its = map ( (++) ("G" ++ myShow (iD t) ++ "x") . show)
                  [ 0 .. ( fromMaybe 0 (Map.lookup (iD t) counts) ) ]
        ig = "G" ++ myShow (iD (fst g)) ++ "x" ++ show (snd g)
        color t = case wt t of
            (1) -> " [color=blue]"
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
