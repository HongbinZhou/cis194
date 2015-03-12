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

-- test data: (((0,1) (2,3)),4)
lj0 = (Single (Size 0) "lj0")
lj1 = (Single (Size 1) "lj1")
lj2 = (Single (Size 2) "lj2")
lj3 = (Single (Size 3) "lj3")
lj4 = (Single (Size 4) "lj4")
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
