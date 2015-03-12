module JoinList where

import Data.Monoid

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
