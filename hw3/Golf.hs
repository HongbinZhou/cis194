{-# LANGUAGE TemplateHaskell #-}
module Golf where

-- ex1
skips :: [a] -> [[a]]
skips a = map (getStrByIdx a) [1 .. length a]

getStrByIdx :: [a] -> Int -> [a]
getStrByIdx s 1 = s
getStrByIdx s n = map (s !!) $ genIdx n $ length s

genIdx :: Int->Int->[Int]
genIdx 1 _ = []
genIdx step len = map (subtract 1) [step, step*2 .. len]

-- test
str1 = "abcd"


