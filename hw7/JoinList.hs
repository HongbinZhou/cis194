module JoinList where

import Data.Monoid
import Sized

-- ex1
data JoinList m a = Empty 
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
     deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append m a b
      where m = (tag a) <> (tag b)

-- test --- 
-- tag (Single 1 a) not work! 1 should be Monoid type.
-- tag (Append (Sum 1) Empty Empty)

-- ex2
-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
       | n == 0 = Just a
       | otherwise = Nothing
indexJ n (Append m left right)
       | n < 0 || n >= rootSize = Nothing
       | n >= leftSize = indexJ (n - leftSize) right
       | n < leftSize = indexJ n left
         where rootSize = getSize m
               leftSize = getSize (tag left)
-- test indexJ
test_indexJ = (map (\x -> indexJ x lj) [0..5]) == (map ((jlToList lj) !!?) [0..5])

-- test data: (((0,1) (2,3)),4)
lj0 = (Single (Size 1) "lj0")
lj1 = (Single (Size 1) "lj1")
lj2 = (Single (Size 1) "lj2")
lj3 = (Single (Size 1) "lj3")
lj4 = (Single (Size 1) "lj4")
lj = ((lj0 +++ lj1) +++ (lj2 +++ lj3)) +++ lj4

-- ex2 helper functions
(!!?) :: [a] -> Int -> Maybe a
[]  !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l@(Single _ b)
      | n <= 0 = l
      | otherwise = Empty
dropJ n (Append m left right)      
      | n < 0 || n >= rootSize = Empty
      | n >= leftSize = dropJ (n - leftSize) right
      | n < leftSize = (dropJ n left) +++ right      
          where rootSize = getSize m
                leftSize = getSize (tag left)

test_dropJ = (jlToList (dropJ 2 lj) ) == (drop 2 (jlToList lj))
