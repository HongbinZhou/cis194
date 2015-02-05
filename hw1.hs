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
