-- source: http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- ex1
toDigits :: Integer -> [Integer]
toDigits x
         | x <= 0 = []
         | otherwise = toDigits leftPart ++ [lastDigit]
           where leftPart =  x `div` 10
                 lastDigit = x `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- ex2
-- Double every other number beginning from the left
-- that is, the second, fourth, ..., are doubled.
doubleEveryOtherL :: [Integer] -> [Integer]
doubleEveryOtherL [] = []
doubleEveryOtherL [x] = [x]
doubleEveryOtherL [x, y] = [x, y*2]
doubleEveryOtherL (x:y:xs) = x:y*2:doubleEveryOtherL xs

-- Double every other number beginning from the right
-- that is, the second-to-last, fourth-to-last, ..., are doubled.
doubleEveryOtherR :: [Integer] -> [Integer]
doubleEveryOtherR = reverse . doubleEveryOtherL . reverse

-- ex3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + (sumDigits xs)

-- ex4
-- validate :: Integer -> Bool
validate x
         | value `mod` 10 == 0 = True
         | otherwise           = False
         where value = sumDigits $ doubleEveryOtherR $ toDigits x

-- ex5
-- Given the number of discs and names for the three pegs, hanoi
-- should return a list of moves to be performed to move the stack of
-- discs from the first peg to the second.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 x _ z = [(x, z)]
hanoi n x y z = (hanoi (n-1) x z y) ++ [(x, z)] ++ [(y, z)]

-- ex6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 x _ _ z = [(x, z)]
hanoi4 2 x y _ z = [(x, y), (y, z)]
hanoi4 3 x y y' z = [(x, y), (x, y'), (x, z), (y', z), (y, z)]
hanoi4 n x y y' z = (hanoi4 (n-1) x z y' y) ++ [(x, z)] ++ [(y, z)]
