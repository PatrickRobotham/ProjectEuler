{-
Largest prime factor
Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import Data.List
import Data.Array


num = 600851475143

primesFromToA a b = (if a<3 then [2] else []) 
                      ++ [i | i <- [o,o+2..b], ar ! i]
  where 
    o  = max (if even a then a+1 else a) 3
    r  = floor . sqrt $ fromIntegral b + 1
    ar = accumArray (\a b-> False) True (o,b) 
          [(i,()) | p <- [3,5..r]
                    , let q  = p*p 
                          s  = 2*p 
                          (n,x) = quotRem (o - q) s 
                          q' = if  o <= q  then q
                               else  q + (n + signum x)*s
                    , i <- [q',q'+s..b] ]

main = print $ maximum [x | x <- primesFromToA 2 (floor . sqrt . fromIntegral $ num) , gcd x num == x]
