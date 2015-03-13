{-# LANGUAGE FlexibleInstances #-}
module JoinListBuffer where

import Data.Monoid
import Buffer
import Scrabble
import Sized
import JoinList

instance Buffer (JoinList (Score, Size) String) where
         toString Empty = []
         toString (Single _ a) = a
         toString (Append _ left right) = toString left ++ toString right

         fromString [] = Empty
         fromString s = Single (scoreString s, Size 1) s

         line = indexJ

         replaceLine n l b
                     | line n b == Nothing = b
                     | otherwise = left +++ l' +++ right
                                 where left = takeJ n b
                                       l' = fromString l
                                       right = dropJ n b

         numLines b = getSize $ size $ tag b
         
         value Empty = 0
         value (Single (Score s, _) _) = s
         value (Append (Score s, _) _ _) = s


-- test data: (((0,1) (2,3)),4)
l0 = (Single (Score 1, Size 1) "lj0")
l1 = (Single (Score 2, Size 1) "lj1")
l2 = (Single (Score 3, Size 1) "lj2")
l3 = (Single (Score 4, Size 1) "lj3")
l4 = (Single (Score 5, Size 1) "lj4")
l = ((l0 +++ l1) +++ (l2 +++ l3)) +++ l4
