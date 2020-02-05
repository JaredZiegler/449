module Patterns where

import Test.QuickCheck

square_odd_all :: [Integer] -> [Integer]
square_odd_all (x:xs)
--  | xs == [] && odd x         = [x*x]
--  | xs == [] && not (odd x)   = [x]
    | odd x = [x*x] ++ square_odd_all xs
    | otherwise = [x] ++ square_odd_all xs
square_odd_all [] = []

prop_square_all_length :: [Integer] -> Bool
prop_square_all_length x = (length x) == (length(square_odd_all x))

my_square :: Integer -> Integer
my_square x = x*x

prop_my_square :: Integer -> Bool
prop_my_square x = (my_square x) >= 0

my_cube :: Integer -> Integer
my_cube x = abs(x*x*x)

prop_my_cube :: Integer -> Bool
prop_my_cube x = (my_cube x) >= 0