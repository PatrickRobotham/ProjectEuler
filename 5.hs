{-Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? -}

good :: Int -> Bool
good x = all (\a -> mod x a == 0) [1..20]

main = print $ head [x | x <- [20,40..] , good x]
