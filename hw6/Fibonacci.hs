{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

import Numeric
import Data.Char

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

fibs3 :: [Integer]
fibs3 = 0:1: next fibs3
        where next (a:t@(b:_)) = (a+b): next t

-- ex3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
         show = showN 20
              where showN 0 _ = "\n"
                    showN n (Cons a xs) = (show a) ++ ", " ++ showN (n-1) xs
         
-- ex4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))

-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 0


-- Note: convert int to binary format, calc the tailing zero
--       such as 12 = 0b1100, the ruler result is 2

tailZero :: Integer -> Integer
tailZero n = toInteger $ length leadingZero
           where leadingZero = takeWhile (=='0') (reverse $ showIntAtBase 2 intToDigit n "")

ruler :: Stream Integer
ruler = streamMap tailZero nats

-- ex6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
         fromInteger n = Cons n (streamRepeat 0)
         negate = streamMap (*(-1)) 
         (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
         (*) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0*b0) ((fromInteger a0)*b' + a'*b)

-- ??? can't do int/int??? Only can: floor ((fromInteger a) / (from Integer b))
instance Fractional (Stream Integer) where
         (/) a@(Cons a0 a') b@(Cons b0 b') = Cons x y
             where divByb0 x = floor (fromIntegral x / fromIntegral b0::Double)
                   x =  divByb0 a0
                   y = streamMap divByb0 (a' - (a/b) * b')

fibs4 :: Stream Integer
fibs4 = x / (1 - x - x * x) --- Holy crap this is awesome
