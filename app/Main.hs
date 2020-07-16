module Main where

import Basic_functions
import Subset_product
import Formatting
import Formatting.Clock
import System.Clock
import Formatting.Internal
import System.IO
import Debug.Trace

{- Andrew Shallue, Summer 2020, a project to implement and time number theory functions 

The suggested code for the timings comes from here: 
https://chrisdone.com/posts/measuring-duration-in-haskell/
-}


main :: IO ()
main = do
  print(square_all [1, 2, 3, 4])

  {-
  putStrLn "Hello World"

  print(enumerate_ss [2, 3, 5, 7])
  print(trivial_sp 11 1 [2, 3, 5, 7])

  print(enumerate_modss [] 11)
  print(enumerate_modss [2] 11)
  print(enumerate_modss [2, 3, 5] 11)

  putStrLn " "

  let lst = enumerate_modss [2, 3, 5] 11
  let sorted = mod_quicksort lst

  print(sorted)

  putStrLn " "
  print(mod_quicksearch sorted 8) 
  
  -}


-- test function for trace
square_all :: [Integer] -> [Integer]
square_all [] = []
square_all (x:xs) = 
  traceShow x $ (x*x) : square_all xs