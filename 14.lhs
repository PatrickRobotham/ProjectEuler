The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.


We use memoization to solve this problem.

> import Debug.Trace
> import Data.Map as Map
> import Control.Monad.State.Lazy as State
> 

> type StateMap a b = State (Map a b) b
> 
>
> memoizeM t x = f x where
>   g x = do
>     y <- t f x  
>     m <- get
>     put $ Map.insert x y m
>     newM <- get
>     return y
>   f x = get >>= \m -> maybe (g x) return (Map.lookup x m)
> 

> c0 = singleton 1 1
> nextCollatz n
>  | mod n 2 == 0 = div n 2
>  | mod n 2 == 1 = 3*n + 1
>
> collatzM :: Monad m => (Integer -> m Integer) -> Integer -> m Integer
> collatzM f' 1 = return $ 1
> collatzM f' n =
>   do
>     a <- f' (nextCollatz n)
>     return $ a+1
> 
> collatz ::Integer -> StateMap Integer Integer
> collatz = memoizeM collatzM
> million = 1000000
> data CWorld = CWorld {champ :: Integer, table :: Map Integer Integer}
> 
> main = do
>   let x = mapM collatz [1..1000000]
>   let y = execState x c0
>   print $ maximumBy (\(a,b) (c,d) -> compare b d) (Map.assocs y)
