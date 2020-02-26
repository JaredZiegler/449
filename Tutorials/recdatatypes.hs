module RecDataTypes where

data LinkedList = EmptyList | Node Integer LinkedList deriving Show

a :: LinkedList
a = Node 5 (Node 6 (Node 7 EmptyList))

toLinkedList :: [Integer] -> LinkedList
toLinkedList [] = EmptyList
toLinkedList (x:xs) = (Node x (toLinkedList xs))

--converts a LinkedList to list of integers
fromLinkedList :: LinkedList -> [Integer]
fromLinkedList EmptyList = []
fromLinkedList (Node value next) = [value] ++ fromLinkedList next

--checks if a given integer is present in a LinkedList
isMember :: Integer -> LinkedList -> Bool
isMember _ EmptyList = False
isMember n (Node value next) = value == n || isMember n next
--isMember n (Node value next)
--    | value == n      = True
--    | otherwise       = isMember n next

--counts how many times a given integer is present in a LinkedList then returns the total
count_Times :: Integer -> LinkedList -> Integer
count_Times _ EmptyList = 0
count_Times n (Node value next)
    | n == value    = 1 + (count_Times n next)
    | otherwise     = count_Times n next

--sum of all elements in the LinkedList     
my_sum :: LinkedList -> Integer
my_sum EmptyList = 0
my_sum (Node value next) = value + my_sum next

--first integer as input is node to add after, second integer is the number to be inserted
--takes integers to check and add a number after the other
insert_after :: Integer -> Integer -> LinkedList -> LinkedList
insert_after _ _ EmptyList = EmptyList
insert_after n y (Node value next)
    | n == value    = (Node value (Node y next))    -- =(Node value (Node y (insert_after n y next))) to insert after every n
    | otherwise     = (Node value (insert_after n y next))

--inserts a node at the start of a list
insert_start :: Integer -> LinkedList -> LinkedList
--insert_start n EmptyList = (Node n EmptyList) -- not nedded because x can map to an empty list
insert_start n x = (Node n x)

--iinserts a node at the end of a LinkedList  
insert_end :: Integer -> LinkedList -> LinkedList
insert_end n EmptyList = (Node n EmptyList)
insert_end n (Node value next) = (Node value (insert_end n next))

--removing the first occurence of the given integer in the LinkedList  
remove_first :: Integer -> LinkedList -> LinkedList
remove_first _ EmptyList = EmptyList
remove_first n (Node value next)
    | n == value    = next
    | otherwise     = (Node value (remove_first n next))

--removes all instances of an integer from the LinkedList    
remove_all :: Integer -> LinkedList -> LinkedList
remove_all _ EmptyList = EmptyList
remove_all n (Node value next)
    | n == value    = remove_all n next
    | otherwise     = (Node value (remove_all n next))