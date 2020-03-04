module HighOrder where

import Data.Char

--a3 tip
data Foo = Token String | TokenOR Foo Foo

divisors :: Integer -> [Integer]
divisors x = filter is_divisor [1..x]
    where is_divisor n = ( x `mod` n) == 0
    
is_prime :: Integer -> Bool
is_prime x
    | length (divisors x) > 2   = False
    | otherwise                 = True
    
list_isPrime :: [Integer] -> [Integer]
list_isPrime x = filter is_prime x

no_primes :: [Integer] -> [Integer]
no_primes x = filter (not . is_prime) x

any_prime :: [Integer] -> Bool
any_prime x = any (is_prime) x

all_prime :: [Integer] -> Bool
all_prime [] = False
all_prime x = all (is_prime) x

all_upper :: String -> Bool
all_upper x = all isUpper x

any_lower :: String -> Bool
any_lower x = any isLower x

is_ordered :: (Integer, Integer) -> Bool
is_ordered (x,y) = y >= x

increasing_order :: [Integer] -> Bool
increasing_order x = all is_ordered (zip x (tail x))

my_sum :: [Integer] -> Integer
my_sum xs = foldr (+) 0 xs
--0 is the starting value for the accumulator
--will then add with (+) the last element of xs to 0 and continue

biggest :: Integer -> Integer -> Integer
biggest a b 
    | a > b     = a
    | otherwise = b

my_max :: [Integer] -> Integer
my_max xs = foldl biggest 0 xs

--foldl first argument is the accumulator, second is from the list
--foldr ifrst argument is from the list, second is the accumulator

sum_one :: Integer -> Integer -> Integer
sum_one i x = i + 1

my_length :: [Integer] -> Integer
my_length xs = foldl sum_one 0 xs