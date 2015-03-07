{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

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

compile :: String -> Maybe Program
compile x = (parseExp lit add mul x) :: Maybe Program

-- ex6
class HasVars a where
      var :: String -> a

data VarExprT = VLit Integer
              | VMul VarExprT VarExprT
              | VAdd VarExprT VarExprT
              | VVar String
     deriving (Show)

instance HasVars VarExprT where
         var x = VVar x

instance Expr VarExprT  where
         lit a = VLit a
         mul x y = VMul x y
         add x y = VAdd x y

-- ???? ex6 not done yet ????

-- instance HasVars (M.Map String Integer -> Maybe Integer) where
--          var x = M.lookup x

-- instance Expr (M.Map String Integer -> Maybe Integer)
--          lit x = Just x
--          mul x y = Just (M.lookup x)

-- test
-- (2+3) * 4
-- main = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20



