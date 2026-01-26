
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = let f1 = fib (n - 1)
            f2 = fib (n - 2)
        in f1 + f2

-- fibs1 :: [Integer]
-- fibs1 = [x <- 1.. | fib x]

-- fibonacci 
fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = foo n 0 1 1
         where foo k a b s = if k == 0 then s
                 else foo (k - 1) ???



                 


           
