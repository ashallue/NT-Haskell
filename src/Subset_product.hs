module Subset_product where

import Basic_functions
import Debug.Trace

-- **************** algorithms for subset-product problem ************

-- Let's start with a trivial enumeration algorithm
-- First input parameter is the modulus n, second target residue a, 
-- third is list of distinct primes
-- output is all subsets which product to a mod n

trivial_sp :: Integer -> Integer -> [Integer] -> [[Integer]]
-- if the list of primes is empty, no subset will satisfy
trivial_sp n a [] = []

-- filter by a function which computes the modular product, compares to a
trivial_sp n a primes = 
  let subset_lst = enumerate_ss primes
  in filter (\lst -> mod_prod n lst == a) subset_lst


{- Next we'll do a standard birthday algorithm, sqr root time-space tradeoff
   Break the list in two, enumerate all subsets of one half, sort, then search 

   To split the list, do splitAt (length lst `div` 2) lst.  Returns a pair, so use fst, snd
-}


twoset_sp :: Integer -> Integer -> [Integer] -> [ModSubset]
twoset_sp n a [] = []
twoset_sp n a primelst = 
  let splitlst  = splitAt (length primelst `div` 2) primelst
      front_ss  = mod_quicksort $ enumerate_modss (fst splitlst) n
      back_ss   = enumerate_modss (snd splitlst) n
  in aux front_ss back_ss []
     
     -- action will take place in this function. First list is sorted, second is not
     -- third parameter will be built up of Subsets that product to a, then returned
     where aux :: [ModSubset] -> [ModSubset] -> [ModSubset] -> [ModSubset]
           --trace ("inside aux with list " ++ show lst1) (aux sorted_lst lst1 lst2)
           aux sorted_lst [] sol_lst = sol_lst
           aux sorted_lst (s:ss) sol_lst 
             -- check if a * b^-1 is in sorted list.  If a * b^-1 = c, then a = bc
             | (subset found_ss) /= [] = aux sorted_lst ss (solution : sol_lst)
             | otherwise               = aux sorted_lst ss sol_lst
             where found_ss = mod_quicksearch sorted_lst (a * (mod_inverse (Subset_product.product s) n))
                   solution = combine s found_ss


-- *************** Helper functions ***************** --

-- A datatype that pairs a subset with its subset product
data ModSubset = 
  ModSubset { subset :: [Integer]
            , modulus :: Integer
            , product :: Integer
            } deriving (Show)

-- Add a new number to the subset and update the product
add_num :: Integer -> ModSubset -> ModSubset
add_num p (ModSubset s m prod) = 
  ModSubset (p:s) m (p * prod `rem` m)

add_num' :: ModSubset -> Integer -> ModSubset
add_num' (ModSubset s m prod) p =
  ModSubset (p:s) m (p * prod `rem` m)

-- append subsets (assume distinct), multiply products, keep n the same
combine :: ModSubset -> ModSubset -> ModSubset
combine (ModSubset s1 n1 prod1) (ModSubset s2 n2 prod2) = 
  ModSubset (s1 ++ s2) n1 (prod1 * prod2 `rem` n1)

-- enumerate all subsets.  Assuming input list is made up of distinct primes
enumerate_ss :: [Integer] -> [[Integer]]
enumerate_ss [] = [[]]
enumerate_ss (p:ps) = 
  -- we combine two lists.   First, all subsets of ps, generated through recursive call
  -- second, that list multiplied by the first prime
  let sub_lst = enumerate_ss ps
      mult_sub_lst = map ((:) p) sub_lst
  in sub_lst ++ mult_sub_lst

-- product a list of integers and return that product modulo n
mod_prod :: Integer -> [Integer] -> Integer

-- using fold, continually multiply and reduce mod n, starting with 1 as the initial product
mod_prod n lst = foldl (\x y -> rem (x * y) n) 1 lst

-- enumerate all subsets, store them as instances of ModSubset.  Second parameter is modulus
enumerate_modss :: [Integer] -> Integer -> [ModSubset]

-- first base case: if list empty, return empty
enumerate_modss []     n = []
-- second base case: if single elt, return the two corresponding subsets
enumerate_modss (p:[]) n = [ModSubset [p] n (rem p n), ModSubset [] n 1]
-- for recursive call, create two lists and combine them
enumerate_modss (p:ps) n = 
  let sub_lst      = enumerate_modss ps n
      mult_sub_lst = map (add_num p) sub_lst
  in sub_lst ++ mult_sub_lst

-- sort a list of type [ModSubset].  Sort by the product
mod_quicksort :: [ModSubset] -> [ModSubset]

-- if empty there is nothing to sort
mod_quicksort [] = []
-- if one element, simply return the same list
mod_quicksort (s:[]) = (s:[])
-- if more than one, first break into two lists and sort each
mod_quicksort lst = 
  let splitlst = splitAt (length lst `div` 2) lst
      front    = mod_quicksort $ fst splitlst
      back     = mod_quicksort $ snd splitlst
  -- then merge the two sorted lists
  in merge front back
     -- define the merge function
     where merge :: [ModSubset] -> [ModSubset] -> [ModSubset]
           merge [] lst2 = lst2
           merge lst1 [] = lst1
           -- now we know both lists are nonempty.  Compare front elements and recurse
           merge (ModSubset s1 n1 prod1:ss) (ModSubset s2 n2 prod2:ts)
              |  prod1 <= prod2 = ModSubset s1 n1 prod1 : (merge ss (ModSubset s2 n2 prod2:ts))
              |  otherwise      = ModSubset s2 n2 prod2 : (merge (ModSubset s1 n1 prod1:ss) ts) 


-- search a list of type [ModSubset] for a particular product.  The list must be sorted by product.
-- This function only returns one such solution if found.  There could be others not returned.
mod_quicksearch :: [ModSubset] -> Integer -> ModSubset

-- if list is empty, return the empty ModSubset
mod_quicksearch [] a = ModSubset [] 2 1

-- otherwise, find the middle of the list, compare, and keep searching
mod_quicksearch lst a 
  | a == midvalue = head back
  | a < midvalue  = mod_quicksearch front a
  | otherwise     = mod_quicksearch back a
  where splitlst = splitAt (length lst `div` 2) lst
        front    = fst splitlst
        back     = snd splitlst
        midvalue = Subset_product.product (head back)
   