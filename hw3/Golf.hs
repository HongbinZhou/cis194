module Golf where

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

-- test
str1 = "abcd"
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

