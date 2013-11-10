{-Maximum path sum II
Problem 67

By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)
-}


-- We use dynamic programming to solve this algorithm

main = do
  contents <- readFile "triangle.txt"
  let
    triangle = map (map read) . map words . lines $ contents :: [[Int]]
    q67 acc (-1) = head acc 
    q67 acc n = q67
                [(triangle !! n) !! x + max (acc !! x) (acc !! (x+1)) |
                 x <- [0..length (triangle !! n) - 1]]
                (n-1)
  
  print $ q67 (triangle !! 99) 98
