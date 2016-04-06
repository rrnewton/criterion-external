-- | Run a simple loop in the IO monad.

module Main where

import System.Environment (getArgs)

main :: IO ()
main =
  do [ns] <- getArgs
     let loop :: Int -> IO ()
         loop 0 = return ()
         loop n = loop (n-1)
     loop (read ns)
