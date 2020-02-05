module HelloHaskell where

something :: Integer
something = 42

same :: Integer -> Integer
same n = n

multiply :: Integer -> Integer -> Integer
multiply x y = x*y

my_odd :: Integer -> Bool
my_odd x = (mod x 2) == 1   --remainder of x divded by 2
--x `mod` 2		//same thing just a different way to write it

multiply_if_odd :: Integer -> Integer -> Integer
multiply_if_odd x y
    |(my_odd x) || (my_odd y) = x*y
    | otherwise = 0

countdown :: Integer -> Integer
countdown x
    | x > 0    = countdown (x-1)
    | x == 0   = 0

sum_to :: Integer -> Integer -> Integer -> Integer
sum_to x y acc
    | x < y    = sum_to (x+1) y (acc+x)
    | x == y   = acc + x
    |otherwise = 0

real_sum_to :: Integer -> Integer -> Integer
real_sum_to x y = sum_to x y 0