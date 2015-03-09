module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- fibs2 :: [Integer]
fibs2 0 = [0]
fibs2 1 = [1,0]
fibs2 n = (x+y):xs
       where xs = fibs2 (n-1)
             x = head xs
             y = head $ tail xs

fibs2' = map fibs2 [0..]
