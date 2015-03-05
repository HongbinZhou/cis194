
module Calc where

import ExprT
import Parser

-- ex1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- ex2
-- evalStr :: String -> Maybe Integer
evalStr x = evalMaybe (parseExp Lit Add Mul x)
        where evalMaybe Nothing = Nothing
              evalMaybe (Just a) = Just (eval a)

-- another version. Use fmap for functor.
evalStr' x = fmap eval (parseExp Lit Add Mul x)


-- ex3
class Expr a where
      lit :: Integer -> a
      mul :: a -> a -> a
      add :: a -> a -> a


instance Expr ExprT where
         lit a = Lit a
         mul x y = Mul x y
         add x y = Add x y

instance Expr Integer where
         lit = id
         add = (+)
         mul = (*)

instance Expr Bool where
         lit = (>0)
         add = (||)
         mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
         lit = MinMax
         add (MinMax a) (MinMax b) = MinMax (max a b)
         mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
         lit x = Mod7 (x `mod` 7)
         add (Mod7 a) (Mod7 b) = Mod7 ( (a+b) `mod` 7)
         mul (Mod7 a) (Mod7 b) = Mod7 ( (a*b) `mod` 7)


-- test
-- (2+3) * 4
main = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


