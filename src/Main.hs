-- | 

module Main where

import Criterion.Main
import Criterion.Internal
import System.Environment

gogo cmd args =
  Benchmarkable \ reps ->
    system $ cmd ++" "++ show reps ++" "++args

main =
 do (cmd : args) <- getArgs
    defaultMain [ Benchmark cmd (gogo cmd args) ]


