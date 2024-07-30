-- https://www.seas.upenn.edu/~cis1940/spring13/hw/03-rec-poly.pdf
module Golf where

-- task 1
skips :: [a] -> [[a]]
skips x = map (`skipsN` x) [1..length x]

skipsN :: Int -> [a]  -> [a]
skipsN num x = map fst (filter (\n -> snd n `mod` num == 0) (zip x [1..length x]))

-- task2
localMaxima :: [Integer] -> [Integer]
localMaxima l
    | length l < 3 = []
    | otherwise = localMaxima3 l

localMaxima3 :: [Integer] -> [Integer]
localMaxima3 (x:y:z:l) = if y > x && y > z then y:localMaxima (y:z:l) else localMaxima (y:z:l)

-- task3
histogram :: [Integer] -> String
histogram arr = drawLine (maxList (countNumbers arr)) (countNumbers arr) ++ "0123456789\n"

drawLine :: Integer -> [Integer] -> String
drawLine 0 arr = []
drawLine maxx arr = map (\e -> if e == maxx then '*' else ' ') arr
    ++ "\n" ++ drawLine (maxx - 1) (subtractMax maxx arr)

subtractMax :: Integer -> [Integer] -> [Integer]
subtractMax maxx = map (\e -> if e == maxx then e - 1 else e)

countNumbers :: [Integer] -> [Integer]
countNumbers x = map (`countN` x) [0..9]

countN :: Integer -> [Integer] -> Integer
countN n = foldr (\ x -> (+) (if n == x then 1 else 0)) 0

maxList :: [Integer] -> Integer
maxList [x] = x
maxList (x:xs) = max x (maxList xs)