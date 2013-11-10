{-
Special Pythagorean triplet
Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

square x = x*x

pythagorean :: Int -> Int -> Bool
-- computes whether the numbers provided can be part of a pythagorean triple
pythagorean a b =
  let c2 = fromIntegral $ square a + square b
      c = sqrt c2 in
  c == fromIntegral (floor c)

pythagoras :: Int -> Int -> Int
-- computes c given a,b. Assumes pythagorean a b
pythagoras a b = let
  c2 = fromIntegral $ square a + square b
  in
   floor . sqrt $ c2


triples = [(a,b,pythagoras a b) | a <- [1..1000], b <- [1..1000], pythagorean a b]

main = print $ head [a*b*c | (a,b,c) <- triples, a+b+c == 1000]
