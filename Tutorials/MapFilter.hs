module MapFilter where

import Data.Char

cube :: Integer -> Integer
cube x = x^3

cubify :: [Integer] -> [Integer]
--cubify x = map cube x 
cubify x = map (^3) x

positive :: [Integer] -> [Integer]
positive x = filter (>0) x

square_odd :: [Integer] -> [Integer]
square_odd x = map (^2) (filter (odd) x)

--helper for square_odd_all
double_if_odd :: Integer -> Integer
double_if_odd x
    | odd x     = x^2
    | otherwise = x
--returns a list where all odd numbers are squared
square_odd_all :: [Integer] -> [Integer]
square_odd_all x = map double_if_odd x 

multiplication_line :: Integer -> [Integer]
multiplication_line x = map (*x) [1..10]
--multiplication_line x = map multiply [1..10] where multiply y = x*y

multiplication_table :: [Integer] -> [[Integer]]
multiplication_table x = map multiplication_line x

lengths :: [String] -> [Integer]
lengths x = map toInteger (map length x)

--removes all capital letters from a string
wc :: String -> String
wc x = filter (not . isUpper) x
