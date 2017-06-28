{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Benchmark for processes that support a simple interactive
-- protocol on stdin for launching benchmarks.

module Main where

import Criterion.Main as C
import Criterion.Types (Benchmarkable(..))
import System.Environment
import System.Process
import Data.List.Split (splitOn)
import System.IO
import GHC.Generics
import Control.DeepSeq


printHelp :: String -> IO () -> IO ()
printHelp progName defMain = do 
  putStrLn $ progName ++" usage:"
  putStrLn $ "  "++progName ++" <cmdname> <cmdArg> ... -- <criterionArg> ..."
  putStrLn $ " "
  putStrLn $ " Benchmark an external program that supports an interactive protocol"
  putStrLn $ " on stdin.  Here, <cmdname> expects N args provided on the command line."
  putStrLn $ " After <cmdname> has started, it blocks on its stdin, waiting for input "
  putStrLn $ " that begins a protocol: "
  putStrLn $ " "
  putStrLn $ "  < READY"
  putStrLn $ "  > START_BENCH <repetition-count>"
  putStrLn $ "  < END_BENCH"
  putStrLn $ "  ..."
  putStrLn $ "  > EXIT"
  putStrLn $ " "
  putStrLn $ " After the child process indicates that it is ready, it goes into a loop"
  putStrLn $ " running batches.  On each batch, it runs the benchmark for the given number "
  putStrLn $ " of repititions, responding with the END_BENCH tag on the child processes' stdout."
  putStrLn $ " "
  putStrLn $ " The benchmark harness times the interval between sending the start"
  putStrLn $ " message and receiving the end message.  The benchmark harness issues" 
  putStrLn $ " many such 'rounds' of timing, before finally closing the subprocess"
  putStrLn $ " with the EXIT message."
  putStrLn $ " "
  putStrLn $ " Standard Criterion usage below: "
  putStrLn $ "================================="
  putStrLn $ " "
  withArgs ["-h"] $ defMain

-- | Read and echo lines until finding the expected one
rdUntil :: Handle -> String -> IO ()
rdUntil h str = do
  ln <- hGetLine h
  if ln == str
    then return ()
    else do putStrLn $" [child] "++ln
            rdUntil h str
             
gogo :: Env -> Benchmarkable
gogo Env{toChild, fromChild} =
  Benchmarkable $ \ reps -> do
--    putStrLn "Starting iteration!"
    hPutStrLn toChild $ "START_BENCH "++show reps
    hFlush toChild
--    putStrLn "Waiting on child"
    rdUntil fromChild "END_BENCH" 
--    putStrLn "Iteration finished"
    return ()

data Env = Env { pid :: ProcessHandle
               , toChild   :: Handle
               , fromChild :: Handle
               }
--  deriving Generic

instance NFData Env where
    rnf Env{pid,toChild,fromChild} =
        pid `seq` toChild `seq` fromChild `seq` ()

initEnv :: String -> [String] -> IO Env
initEnv cmd args = do     
  (Just sin, Just sout, _, ph) <- createProcess ((proc cmd args)
                                  { std_in = CreatePipe, std_out = CreatePipe })
  rdUntil sout "READY"
  return Env{ pid= ph, toChild= sin, fromChild= sout }

cleanEnv :: Env -> IO ()
cleanEnv Env{pid} = do
  terminateProcess pid
  _ <- waitForProcess pid
  return ()

main :: IO ()
main =
 do allargs <- getArgs
    prog    <- getProgName
    let gomain cmd args = defaultMain [
                            C.env -- TODO: envWithCleanup
                             (initEnv cmd args)
                             (\ev -> bench cmd (gogo ev))
                          ]
    case allargs of
      (cmd:ls) | any (== "-h") ls -> printHelp prog (gomain cmd [])
      ls       | any (== "-h") ls -> printHelp prog (gomain "" [])
      _ -> 
       case splitOn ["--"] allargs of 
         [] -> error $ prog++": expects the first argument to be a script/binary name."
         [(cmd : args)]     -> withArgs []   $ gomain cmd args
         [(cmd:args), rest] -> withArgs rest $ gomain cmd args
         _ -> error $ prog++ ": error, got more than one '--' in arguments:\n  "++unwords allargs

