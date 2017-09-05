module Analysis where

import Types

data TaggedGene = TaggedGene {      geneID :: ID
                    ,   thres :: Thres
                    ,   genSt :: GeneState } deriving (Show, Read, Eq)
