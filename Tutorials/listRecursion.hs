module ListRecursion where

my_length :: [a] -> Integer
my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_factorial :: Integer -> Integer
my_factorial 0 = 1
my_factorial n = n * my_factorial (n-1)

my_product :: [Integer] -> Integer
my_product [] = 1
my_product (x:xs) = x * my_product (xs)

my_reverse :: [a] -> [a]
my_reverse [] = []
my_reverse (x:xs) = my_reverse (xs) ++ [x]

my_zip :: [a] -> [b] -> [(a,b)]
my_zip [] _ = []
my_zip _ [] = []
my_zip (x:xs) (y:ys) = [(x,y)] ++ my_zip xs ys

my_replicate :: Integer -> a -> [a]
my_replicate 0 _ = []
my_replicate n x = [x] ++ my_replicate (n-1) x

find_prev :: [Integer] -> Integer -> Integer
find_prev [] _ = (-1)
find_prev (x:xs) n 
    | xs == []       = (-1)
    | (head xs) == n = x
    | otherwise      = find_prev xs n 
    
elem_num ::  Integer -> [Integer] -> Integer
elem_num _ [] = 0
elem_num n (x:xs)
    | x == n       = 1 + elem_num n xs
    | otherwise    = elem_num n xs