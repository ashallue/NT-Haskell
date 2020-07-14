module Basic_functions where

-- Note: module name must be capitalized, then load using :l filename or :l modulename

-- #######  Introduction ######
-- This file contains some simple number theory functions
-- Andrew Shallue, July 2019

-- for random numbers, I'm using System.Random from 
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms

-- Oops, I need to import code.  See here:
--  http://seanhess.github.io/2015/08/17/practical-haskell-importing-code.html

-- Directory path for packages on my Windows machine is the following:
-- C:\Users\Andrew Shallue\AppData\Roaming\cabal\packages\hackage.haskell.org\

import System.Random as R

-- simple recursive gcd function
mygcd :: Integer -> Integer -> Integer
mygcd x 0 = x
mygcd 0 x = x
mygcd x y = mygcd y (rem x y)

-- extended gcd.  Given x,y it finds a,b such that ax + by = gcd(x,y)
-- for my formulas I am following wikipedia: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
-- for this to work, x and y must be non-negative
myexgcd :: Integer -> Integer -> (Integer, Integer)
myexgcd x y = aux x y 1 0 0 1
  where aux :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
        aux x 0 s0 t0 s1 t1 = (s0, t0)
        aux 0 x s0 t0 s1 t1 = (s1, t1)
        aux x y s0 t0 s1 t1 = aux y (rem x y) s1 t1 (s0 - quo * s1) (t0 - quo * t1) 
          where quo = div x y

-- multiplicative inverse.  First param is a, second is n, returns a^(-1) mod n
-- if a is not a unit, returns 0
mod_inverse :: Integer -> Integer -> Integer
mod_inverse a n
  | mygcd a n /= 1 = 0
  | otherwise = let inv = fst $ myexgcd a n
                in if   inv < 0
                   then inv + n
                   else inv

-- non-modular exponentiation.  binary method,recursive  Non-modular so I can test bigints
-- got it, Integer is arbitrary-precision, Int fixed at machine word size
rec_exp :: Integer -> Integer -> Integer
rec_exp a 0 = 1
rec_exp 0 e = 0
rec_exp 1 e = 1
rec_exp a e 
  -- if exp even, do (a^{e/2})^2.  If exp odd, do a * a^{e-1}
  | (rem e 2) == 0 = let subproduct = rec_exp a (div e 2) in
                     subproduct * subproduct 
  | (rem e 2) == 1 = a * rec_exp a (e-1)

-- modular exp.  Binary method, recursive.  Not tail-recursive.  return a^e mod n
-- currently not restricting modulus to be positive.  
-- see https://stackoverflow.com/questions/11910143/positive-integer-type for ideas on how to fix
rec_mod_exp :: Integer -> Integer -> Integer -> Integer
rec_mod_exp _ 0 _ = 1
rec_mod_exp 0 _ _ = 0
rec_mod_exp a e n 
  | (rem e 2) == 0 = let subproduct = rec_mod_exp a (div e 2) n in
                   subproduct * subproduct `rem` n
  | (rem e 2) == 1 = a * rec_mod_exp a (e-1) n `rem` n

-- helper function for tail-recursive modular exp.  I need to convert exp e to binary
-- most significant digit is at the head of the list
-- The auxiliar function is weird because powers of 2 are a special case
intToBinary :: Integer -> [Int]
intToBinary 0 = [0]
intToBinary n = aux n []
  where aux 0 lst = lst
        -- the next most sig bit is either 0 or 1, depending on rem when divided by 2
        aux e lst 
          | (rem e 2) == 0 = aux (div e 2) (0:lst)
          | otherwise      = aux (div (e-1) 2) (1:lst)


-- modular exp, tail-recursive version.  Binary exp algorithm
-- Computes a^e mod n.  
-- BUG REPORT: currently goes into infinite loop if you give it a negative exponent
mod_exp :: Integer -> Integer -> Integer -> Integer
mod_exp _ 0 _ = 1
mod_exp 0 _ _ = 0
-- the 4th param accumulates the power, e has been replaced with bit representation of e
mod_exp a e n = aux a (intToBinary e) n 1 
  where aux a [] n acc = acc
        aux a (bit:bits) n acc 
          | bit == 0 = aux a bits n (rem (acc * acc) n)
          | bit == 1 = aux a bits n (rem (acc * acc * a) n)

-- Using the random package, I can generate uniform random numbers modulo n
unifDist :: Int -> IO Int
unifDist n = getStdRandom (randomR (1,n))

-- I want to be able to add pairs of numbers in the obvious way
pair_sum :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
pair_sum p1 p2 = ( (fst p1) Prelude.+ (fst p2), (snd p1) Prelude.+ (snd p2) )


-------------- Prime examples
p = 12131072439211271897323671531612440428472427633701410925634549312301964373042085619324197365322416866541017057361365214171711713797974299334871062829803541
     
q = 12027524255478748885956220793734512128733387803682075433653899983955179850988797899869146900809131611153346817050832096022160146366346391812470987105415233


-- primality test to the base 2
-- returns true if 2^(p-1) = 1 mod p, false otherwise
isPrime2Fermat :: Integer -> Bool
isPrime2Fermat n = and [(mygcd 2 n == 1), (mod_exp 2 (n-1) n == 1)]


-- Fermat primality test to the first 10 prime bases
-- False means provably composite.  True means probably prime, specifically that n passed all bases
isPrimeFermat :: Integer -> Bool
-- aux function will store trial results in the list, then apply and at end
isPrimeFermat n = 
  --aux :: Integer -> [Integer] -> [Bool] -> Bool
  aux n [2,3,5,7,11,13,17,19,23,29] []
  where aux n [] reslst = and reslst
        aux n baselst reslst = 
            let a      = head baselst
                result = and [(mygcd a n == 1), (mod_exp a (n-1) n == 1)] 
            in
                aux n (tail baselst) (result : reslst)

-- a helper function for Miller-Rabin-Selfridge, which decomposes n as 2^r * d where d odd
odd_even_decompose :: Integer -> (Integer, Integer)
odd_even_decompose n 
  -- if n is odd, return (0,n)
  | rem n 2 == 1 = (0, n)
  -- otherwise, decompose n/2, and add 1 to the even part using pair_sum
  | otherwise    = pair_sum (1,0) $ odd_even_decompose (div n 2)


-- Miller-Rabin-Selfridge test to the base b (also known as strong-base-b pseudoprime)
-- True means probably prime, False means provably composite

isPrimeStrong :: Integer -> Integer -> Bool
isPrimeStrong n b = 
  -- if n is even, return False
  if rem n 2 == 0 
    then False 
    -- compute n-1 = 2^r * d where d is odd
    else let (r,d) = odd_even_decompose (n-1) 
         in aux n r (mod_exp b d n)

         -- aux function recursively checks the strong psp condition, starting with b^d mod n
         -- For the base case, we return True if b^d mod n is either 1 or -1       	
         where aux n 0 power = or [power == 1, power == -1]
               -- For the recursive call, square, decrease r, and check whether we get -1
               aux n r power = or [power == -1, aux n (r-1) (mod_exp power 2 n)]
