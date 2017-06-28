-- | Benchmark for batch, one-shot processes.

module Main where

import Criterion.Main
import Criterion.Types (Benchmarkable(..))
import System.Environment
import System.Process
import Data.List.Split (splitOn)


gogo cmd args =
  Benchmarkable $ \ reps ->
    do _ <- system $ cmd ++" "++ show reps ++" "++unwords args
       return ()


printHelp :: String -> IO () -> IO ()
printHelp progName defMain = do 
  putStrLn $ progName ++" usage:"
  putStrLn $ " "
  putStrLn $ "  "++progName ++" <cmdname> <cmdArg>^N -- <criterionArgs>"
  putStrLn $ " "
  putStrLn $ " Benchmark an external program that takes a repetition count." 
  putStrLn $ " Above, <cmdname> expects N+1 args, with the first argument being "
  putStrLn $ " an iteration count.  Each measurement is the complete time of the"
  putStrLn $ " spawned sub-process."
  putStrLn $ " "
  putStrLn $ " Standard Criterion usage below: "
  putStrLn $ "================================="
  putStrLn $ " "
  withArgs ["-h"] $ defMain

main =
 do allargs <- getArgs
    prog    <- getProgName
    let gomain cmd args = defaultMain [ bench cmd (gogo cmd args) ]
    case allargs of
      (cmd:ls) | any (== "-h") ls -> printHelp prog (gomain cmd [])
      ls       | any (== "-h") ls -> printHelp prog (gomain "" [])
      _ -> 
       case splitOn ["--"] allargs of 
         [] -> error $ prog++": expects the first argument to be a script/binary name."
         [(cmd : args)]     -> withArgs []   $ gomain cmd args
         [(cmd:args), rest] -> withArgs rest $ gomain cmd args
         _ -> error $ prog++ ": error, got more than one '--' in arguments:\n  "++unwords allargs
