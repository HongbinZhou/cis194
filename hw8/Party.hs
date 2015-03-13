module Party where

import Employee
import Data.Monoid

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e gl@(GL l fun)
       | not (e `elem` l) = GL (e:l) (empFun e)
       | otherwise = gl

e0 = Emp "aaa" 10 
e1 = Emp "bbb" 5
e2 = Emp "ccc" 1

test_glCons = glCons e0 (GL [] 0)

instance Monoid GuestList where
         mempty = GL [] 0
         mappend gl0@(GL l0 f0) gl1@(GL l1 f1) = 
                 GL (l0++l1) (f0+f1)

gl0 = mempty :: GuestList
gl1 = glCons e1 gl0
gl2 = glCons e2 gl0
test_append = gl1 <> gl2

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl0@(GL _ f0) gl1@(GL _ f1)
        | f0 > f1 = gl0
        | otherwise = gl1
