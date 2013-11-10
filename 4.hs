{-Largest palindrome product
Problem 4

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}
import Data.List

palindrome :: Int -> Bool
palindrome x = show x == reverse (show x)

main = do
  print $ maximumBy (\(x,y) (a,b) -> compare (x*y) (a*b)) [(x,y) | x <- [999,998 .. 100] , y <- [999,998 .. 100], palindrome $ x*y] 
