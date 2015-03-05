
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

-- test
-- (2+3) * 4
main = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


