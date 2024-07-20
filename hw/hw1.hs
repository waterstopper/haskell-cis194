-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

-- validate card number
doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x:y:zs) = (x * 2) : y : doubleEverySecond zs

sumOfDigits :: Integer -> Integer
sumOfDigits n
    | n == 0    = 0
    | otherwise = (n `mod` 10) + sumOfDigits (n `div` 10)

sumOfDigitsInList :: [Integer] -> Integer
sumOfDigitsInList [] = 0
sumOfDigitsInList (x:xs) = sumOfDigits x + sumOfDigitsInList xs

listLen :: [Integer] -> Integer
listLen [] = 0
listLen (x:xs) = 1 + listLen xs

isValidCardNumber :: [Integer] -> Bool
isValidCardNumber n 
    | listLen n /= 16 = False
    | otherwise = sumOfDigitsInList (doubleEverySecond n) `mod` 10 == 0

-- hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ ((a, b) : hanoi (n-1) c b a)
