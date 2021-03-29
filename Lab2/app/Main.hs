-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import System.Environment
import Data.List
import Control.Monad

import Tr

-- | Main - parse args, and read from stdin.
main :: IO ()
-- main = putStrLn $ tr "ab" (Just "x") "abcd"




main = do
   args <- getArgs
   progName <- getProgName
   str <- getLine
   unless (str == "q") $ do
     if (args !! 0) == "-d"
       then putStrLn $ tr (args !! 1) Nothing str
       else putStrLn $ tr (args !! 0) (Just (args !! 1)) str
     main
