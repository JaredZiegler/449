module FirstAssign where

import Test.QuickCheck

--Jared Ziegler
--30029763
--449 A1

--1 GCD implementation

myGCD :: Integer -> Integer -> Integer
myGCD a b
    | a == 0        = b                 --everything divides zero
    | b == 0        = a                 --everything divides zero
    | a == b        = a                 --same number is the gcd
    | a > b         = myGCD (a-b) b     --if a is larger sub b from a and call recursively
    | otherwise     = myGCD a (b-a)     --if b is larger sub a from b and call recursively

--2    
--5.20 divisors :: Integer -> [Integer]
--returns list of positive integer divisors. 
--Also define a function for primes, isPrime :: Integer -> Bool
--checks wether or not a positive integer is prime, returns true, if not prime returns false
--returns false if not int+

--add x to the list if nmodx is 0 for all numbers less than n
divisors :: Integer -> [Integer]
divisors n = [x | x <-[1..n], n `mod` x == 0]   

--uses divisors and checks if the total amount of divisors other than 1 is equal to 1
isPrime :: Integer -> Bool
isPrime k = length [x | x <-[2..k], k `mod` x == 0] == 1

--3
--5.32 Implement database functions 
--books :: Database -> Person -> [Book]
--borrowers :: Database -> Book -> [Person]
--borrowed :: Database -> Book -> Bool
--numBorrowed :: Database -> Person -> Int
--makeLoan :: Database -> Person -> Book -> Database
--returnLoan :: Database -> Person -> Book -> Database

type Person   = String
type Book     = String

type Database = [(Person, [Book])]

exDB :: Database
exDB = [
        ( "name1" , ["book1","book2"] ),
        ( "name2" , [] ),
        ( "name3" , ["book4"] )
       ]

--extracts a list of list containing books into a single list of books
bookExtract :: [[Book]] -> [Book]
bookExtract bookLoL = [ currentBook | bookList <- bookLoL, currentBook <- bookList ]

--check each person in DB if they match the given person return their book list 
books :: Database -> Person -> [Book]
books database person =  bookExtract ([ bookList | (currentPerson, bookList) <- database, currentPerson == person ])

--check each book in DB if they match the given book return the person
borrowers :: Database -> Book -> [Person]
borrowers database book = [ currentPerson | (currentPerson, bookList) <- database , book `elem` bookList]

--if the there is no borrower for the given book return false, if anyone is borrowing the book return True
borrowed :: Database -> Book -> Bool
borrowed database book
    | (borrowers database book) /= []   = True
    | otherwise                         = False

--returns the amount of books a given person is borrowing
numBorrowed :: Database -> Person -> Int
numBorrowed database person = length (books database person)

makeLoan :: Database -> Person -> Book -> Database
makeLoan database person newBook = [(person, ((books database person) ++ [newBook]))] ++ database

--others :: Database -> Person -> Database
--others = (makeLoan database (newPerson, newBookList) | (currentPerson, bookList) <- database, person /= currentPerson bookList)

returnLoan :: Database -> Person -> Book -> Database
returnLoan database person book = database


--4
--6.8 define function
-- rotate90 :: Picture -> Picture
--rotates a picture 90 degrees clockwise
--use: type Picture = [[Char]]

type Picture = [[Char]]

testPic :: Picture
testPic = [".##.",
           ".#.#",
           ".###",
           "####"]

rotate90 :: Picture -> Picture
rotate90 oG = [reverse [x!!n | x <- oG] | n <- [0..length (oG!!0) - 1]] --for each line, the ith element is taken for the new string


