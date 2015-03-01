module Golf where

-- ex1
skips :: [a] -> [[a]]
skips [] = []
skips a = [a] ++ map getByIdx [2 .. length a]
          where getByIdx n = map (\x -> a !! (x-1)) [n, n*2 .. length a]
                
-- test
str1 = "abcd"


