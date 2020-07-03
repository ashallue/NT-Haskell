module Subset_product where

-- algorithms for subset-product problem

-- Let's start with a trivial enumeration algorithm
-- First input paramete is the modulus n, second is list of distinct primes
-- output is a subset which products to a mod n

trivial_sp :: Integer -> Integer -> [Integer] -> [Integer]
-- if the list of primes is empty, no subset will satisfy
trivial_sp n a [] = []

trivial_sp n a (p:ps) = undefined

-- enumerate all subsets.  Assuming input list is made up of distinct primes
enumerate_ss :: [Integer] -> [[Integer]]
enumerate_ss [] = [[]]
enumerate_ss (p:ps) = 
  -- we combine two lists.   First, all subsets of ps, generated through recurisve call
  -- second, that list multiplied by the first prime
  let sub_lst = enumerate_ss ps
      mult_sub_lst = map ((:) p) sub_lst
  in sub_lst ++ mult_sub_lst