-- | 

module Main where

import Criterion.Main
import Criterion.Types (Benchmarkable(..))
import System.Environment
import System.Process

gogo cmd args =
  Benchmarkable $ \ reps ->
    do _ <- system $ cmd ++" "++ show reps ++" "++unwords args
       return ()

main =
 do (cmd : args) <- getArgs
    defaultMain [
      bench cmd (gogo cmd args)
      ]


