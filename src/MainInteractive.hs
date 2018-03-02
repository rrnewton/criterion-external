{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Benchmark for processes that support a simple interactive
-- protocol on stdin for launching benchmarks.

module Main where

import Criterion.Main as C
import Criterion.Types (Benchmarkable(..), toBenchmarkable)
import System.Environment
import System.Process
import Data.List as L
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
    else do putStrLn $"> "++ln
            rdUntil h str

gogo :: Env -> Benchmarkable
gogo Env{toChild, fromChild, phand} =
  toBenchmarkable $ \ reps -> do
    -- putChar '.'; hFlush stdout -- TODO: verbose mode.
    stat <- getProcessExitCode phand
    case stat of
      Just code -> do -- TODO: echo buffered stdout?
                      error $ "child process exited with code "++show code
      Nothing -> do
        hPutStrLn toChild $ "START_BENCH "++show reps
        hFlush toChild
        -- putChar '-'; hFlush stdout
        rdUntil fromChild "END_BENCH"
        -- putChar '|'; hFlush stdout
        return ()

data Env = Env { phand :: ProcessHandle
               , toChild   :: Handle
               , fromChild :: Handle
               }
--  deriving Generic

instance NFData Env where
    rnf Env{phand,toChild,fromChild} =
        phand `seq` toChild `seq` fromChild `seq` ()


initEnv :: String -> [String] -> IO Env
initEnv cmd args = do
  (Just sin, Just sout, _, ph) <- createProcess ((proc cmd args)
                                  { std_in = CreatePipe, std_out = CreatePipe })
  putStrLn $ "criterion-interactive: created subprocess ("++ unwords (cmd:args)++
             "), now waiting for READY signal."
  rdUntil sout "READY"
  return Env{ phand= ph, toChild= sin, fromChild= sout }

cleanEnv :: Env -> IO ()
cleanEnv Env{phand} = do
  putStrLn "Waiting for child process to terminate."
  hFlush stdout
  terminateProcess phand
  _ <- waitForProcess phand
  return ()

main :: IO ()
main =
 do allargs <- getArgs
    prog    <- getProgName
    let gomain cmd args = do
            defaultMain
              [ C.env -- TODO: envWithCleanup
                (initEnv cmd args)
                (\ev -> bench cmd (gogo ev))
              ]
--            putStrLn "Done benchmarking." -- defaultMain doesn't return!

    -- This is hacky and lame.
    -- The LAST batch of arguments are for criterion:
    case splitOn ["--"] allargs of
         ((cmd:ls):_) | any (== "-h") ls -> printHelp prog (gomain cmd [])
         (ls:_)       | any (== "-h") ls -> printHelp prog (gomain "" [])

         [] -> error $ prog++": expects the first argument to be a script/binary name."
         [(cmd : args)]     -> withArgs [] $ gomain cmd args
         ((cmd:args) : rest) -> do
             let (theirs,ours) = (init rest, last rest)
                 rest' = concat (L.intersperse ["--"] theirs)
                 args' = case rest' of
                           [] -> args
                           _  -> args ++["--"]++ rest'
             withArgs ours $ gomain cmd args'
