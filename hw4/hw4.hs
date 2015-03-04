-- ex1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
     | even n = n + fun2 (n `div` 2)
     | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =  sum . filter even . takeWhile (>1) . iterate f
      where f n 
              | even n = n `div` 2
              | otherwise = 3 * n + 1
-- ex2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf 
         where addNode a Leaf = Node 1 Leaf a Leaf
               addNode a (Node n leftTree b rightTree) 
                         | depth leftTree <= depth rightTree = (Node (n+1) (addNode a leftTree) b rightTree)
                         | otherwise = (Node n leftTree b (addNode a rightTree))
                         where depth Leaf = 0
                               depth (Node n _ _ _) = n
        
-- test
-- foldTree "ABCDEFGHIJ"
