module ListRecursion2 where

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x,x] ++ duplicate xs

range :: Integer -> [Integer]
range 0 = [0]
range n = [n] ++ range (n-1)

member :: Integer -> [Integer] -> Bool
member _ [] = False
member n (x:xs)
    | n == x        = True
    | otherwise     = member n xs

--member n (x:xs) = n == x || member n xs

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]

is_palindrome :: (Eq a) => [a] -> Bool 
is_palindrome [] = False
is_palindrome [_] = True
is_palindrome (x:xs) = x == (last xs) && is_palindrome (init xs) 

find_next :: Integer -> [Integer] -> Integer
find_next _ [] = (-1)
find_next n (x:xs)
    | xs == []      = (-1)
    | x == n        = head xs
    | otherwise     = find_next n xs
 
 my_zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
 my_zip3 [] _ _ = []
 my_zip3 _ [] _ = []
 my_zip3 _ _ [] = []
 my_zip3 (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ my_zip3 xs ys zs