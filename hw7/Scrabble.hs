{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int
        deriving (Eq, Ord, Show, Num)

instance Monoid Score where
         mempty = Score 0
         mappend (Score a) (Score b) = Score (a+b)

score :: Char -> Score
score m
      | n == 'a' = Score 1
      | n == 'b' = Score 3
      | n == 'c' = Score 3
      | n == 'd' = Score 2
      | n == 'e' = Score 1
      | n == 'f' = Score 4
      | n == 'g' = Score 2
      | n == 'h' = Score 4
      | n == 'i' = Score 1
      | n == 'j' = Score 8
      | n == 'k' = Score 5
      | n == 'l' = Score 1
      | n == 'm' = Score 3
      | n == 'n' = Score 1
      | n == 'o' = Score 1
      | n == 'p' = Score 3
      | n == 'q' = Score 10
      | n == 'r' = Score 1
      | n == 's' = Score 1
      | n == 't' = Score 1
      | n == 'u' = Score 1
      | n == 'v' = Score 4
      | n == 'w' = Score 4
      | n == 'x' = Score 8
      | n == 'y' = Score 4
      | n == 'z' = Score 10
      | otherwise = Score 0
        where n = toLower m

scoreString :: String -> Score
scoreString [] = Score 0
scoreString (x:xs) = score x + scoreString xs
