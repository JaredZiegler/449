module SecondAssign where

import Data.Char

--Jared Ziegler
--30029763

testString :: String
testString = "testing123TESTING"

--1.a
--alphaToUpper returns string with all alphabet characters capitalized
alphaToUpper :: String -> String
alphaToUpper xs = [ toUpper x | x <- xs, isAlpha x ]  --for each element of the list, if its a letter capitalize it and add it to string

--1.b
--same as alphaToUpper but with recursion instead of list comp
alphaToUpperRec :: String -> String
alphaToUpperRec "" = ""     --base case, if empty return empty
alphaToUpperRec (x:xs) 
    | isAlpha x == True   = toUpper x : alphaToUpperRec xs      --if head of list is letter, uppercase it and recursively concat the tail
    | otherwise           = alphaToUpperRec xs      --if its not a letter dont add it and continue recursively checking the tail

--2.a
--takes binary in string form and converts it to decimal integer
parseBin :: String -> Integer
parseBin str = parseBinHelp(reverse str)    --flips string for easier reading

parseBinHelp :: String -> Integer
parseBinHelp [] = 0     --basecase empty string =0
parseBinHelp (x:xs) = toInteger (digitToInt x) + 2 * parseBinHelp xs   --takes first element of list and adds 2*the rest of the list with each elmeent increasing the multiplication for digit value

--2.b
--takes an decimal integer and returns the same number in a binary string
encodeBin :: Integer -> String
encodeBin 0 = "0"
encodeBin 1 = "1"
encodeBin n
    | n `mod` 2 == 0 = encodeBin (n `div` 2) ++ "0"    --if the number is even then add a 0
    | otherwise = encodeBin ( n `div` 2) ++ "1"        --if the number is odd then add a 1

--3.a
--takes 2 sorted lists as input, returns a sorted list that is the combination of both inputs
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] x = x     --if one is empty then the result is the otherwise
mergeLists y [] = y
mergeLists (x:xs) (y:ys)
    | x<=y          = x : mergeLists xs (y:ys) --if head of xs is less than head of ys then add head of xs the  recursively call with tail of xs and all of ys
    | y<=x          = y : mergeLists (x:xs) ys

--3.b
--splits a given list into 2, one list has even indexes the other has odds
splitList :: [Integer] -> ([Integer],[Integer])
splitList xs = (evens xs, odds xs)

--gets the even indexes by taking alternating elements from the list
evens :: [Integer] -> [Integer]
evens (x:xs) = x:odds xs
evens _ = []
--gets the odd indexes by taking alternating elements from the list starting on the second element
odds :: [Integer] -> [Integer]
odds (_:xs) = evens xs
odds _ = []

--3.c
--takes an input list and returns a sorted one
mSort:: [Integer] -> [Integer]
mSort [] = []
mSort [x] = [x]
mSort xs = mergeLists (mSort(fst(splitList xs))) (mSort(snd(splitList xs)))
--recursively calls msort until it reaches single elements then merges all the singletons in order