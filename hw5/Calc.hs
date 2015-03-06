{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

-- ex1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- ex2
-- evalStr :: String -> Maybe Integer
evalStr x = evalMaybe (parseExp ExprT.Lit ExprT.Add ExprT.Mul x)
        where evalMaybe Nothing = Nothing
              evalMaybe (Just a) = Just (eval a)

-- another version. Use fmap for functor.
evalStr' x = fmap eval (parseExp ExprT.Lit ExprT.Add ExprT.Mul x)


-- ex3
class Expr a where
      lit :: Integer -> a
      mul :: a -> a -> a
      add :: a -> a -> a


instance Expr ExprT where
         lit a = ExprT.Lit a
         mul x y = ExprT.Mul x y
         add x y = ExprT.Add x y

-- ex4
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


-- ex5
instance Expr Program where
         lit x = [StackVM.PushI x]
         add a b = a ++ b ++ [StackVM.Add]
         mul a b = a ++ b ++ [StackVM.Mul]

-- compile :: String -> Maybe Program
-- This can work now: 
test = (mul (add (lit 1) (lit 2)) (lit 2)) :: Program
-- compile = parseExp StackVM.IVal StackVM.Add StackVM.Mul 

-- test
-- (2+3) * 4
-- main = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20



