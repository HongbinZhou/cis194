module Golf where

import qualified Data.List as L

-- ex1
skips :: [a] -> [[a]]
skips [] = []
skips a = [a] ++ map getByIdx [2 .. length a]
          where getByIdx n = map (\x -> a !! (x-1)) [n, n*2 .. length a]
       
-- ex2, Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:xs) = [y | x<y, z<y] ++ localMaxima (y:z:xs)

-- ex3
histogram :: [Integer] -> String
histogram x = concat $ L.intersperse "\n" $ line x
  where line x =  L.reverse $ L.transpose $ pair x
        pair x = map (\n -> show n ++ fill x !! n) [0..9]
        fill x = map (\n -> replicate n '*' ++ replicate ((maximum $ occur x) - n) ' ') $ occur x
        occur x = map (\n -> length $ filter (==n) x ) [0..9]
                
vshow x = putStrLn $ histogram x

-- test
str1 = "abcd"

-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

-- vshow [99,9,1,2,3,2,3]
