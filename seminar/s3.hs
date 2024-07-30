-- t is type; E is Empty, C is Cons
data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 2 Empty))

lst2 :: List Char
lst2 = Cons 'x' (Cons 'y' (Cons 'z' Empty))

lst3 :: List Bool
lst3 = Cons True (Cons False Empty)

-- self defined filter and map
filterList :: (t -> Bool) -> List t -> List t
filterList _ Empty = Empty
filterList p (Cons x xs)
  | p x       = Cons x (filterList p xs)
  | otherwise = filterList p xs

-- from type a create type b
mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 