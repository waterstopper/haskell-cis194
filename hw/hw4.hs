-- task 1 (rewrite functions in idiomatic style)
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- task2
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
-- foldTree 

-- task3 implement functions

xor :: [Bool] -> Bool
xor = foldr (/=) False

-- append current a[i] to b[] performing f-transformation: b.add(f(a[i]))
-- don't apply map simplification, because the task is to implement map with foldr :)
map' :: (a -> b) -> [a] -> [b]
map' f = map (\ x -> f x)

-- task4 Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter (>3) [1..(n-2) `div` 2 ]


-- don't know why it doesn't work
-- complexNumbers :: Integer -> [Integer]
-- complexNumbers n = map (\(x, y) -> x + y + 2 * x * y) . filter (uncurry (<=)) . cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
