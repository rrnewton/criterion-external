-- | Run a simple loop in the IO monad.

module Main where

import System.Environment (getArgs)
import Control.Exception (evaluate)

main :: IO ()
main =
  do [ns] <- getArgs
     let loop :: Int -> Int
         loop 0 = 99
         loop n = loop (n-1)
     _ <- evaluate (loop (read ns))
     return ()
