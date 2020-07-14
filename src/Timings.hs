module Timings where

import Basic_functions
import Subset_product
import Formatting
import Formatting.Clock
import System.Clock
import Formatting.Internal
import System.IO

-- a function that returns the time taken to run the Fermat function
timeTaken :: Integer -> IO TimeSpec
timeTaken n = do
  start <- getTime Monotonic
  print(isPrime2Fermat n)
  end <- getTime Monotonic
  return $ diffTimeSpec start end
  



-- a function that reads a file line by line and applies timeTaken to it
-- update, I should just use the lines function.  lines :: String -> [String]
-- handle is a file to write to
times :: [String] -> Handle -> IO ()
times [] h = hClose h
times (line:lines) h = do
  print(line)
  time_for_one <- timeTaken $ read line
  hPrint h time_for_one
  times lines h
  

{- Previous version
start <- getTime Monotonic
  print(isPrimeFermat 1001) 
  end <- getTime Monotonic

  -- Having problems with the formatting.  Let's try to work through it slowly.
  -- diffTimeSpec does TimeSpec -> TimeSpec -> TimeSpec
  print( diffTimeSpec start end)
-} 

{- A main function that runs a timing
main = do
  a <- timeTaken 101
  print(a)

  -- this code copied from Learn you a Haskell
  -- open the file in read mode and assign nums as the handle
  nums <- openFile "pows2.txt" ReadMode
  -- get the contents of the handle line by line
  contents <- hGetContents nums
  num_per_line <- return (lines contents)

  -- now open a file to write the corresponding times
  file_for_times <- openFile "times.txt" WriteMode

  -- apply the time take function
  times num_per_line file_for_times  

  -- times file closed already, now close the nums file
  hClose nums
-}