
module Calc where

import ExprT

-- ex1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- test
-- (2+3) * 4
main = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


