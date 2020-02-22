module Puzzles

where

-- | 1. length calculates the lenght of a list using an anonymous functions
-- counts with n
length' :: [a] -> Int
length' = foldr (\_ n -> n + 1) 0

-- | 2. or uses the || operator on the list, basecase is that the list is False
or' :: [Bool] -> Bool
or' = foldr (||) False

-- | 3. elem check elementwise in a list whether the x matches this element.
-- if it does the basecase False will swap to True.
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\a z -> (a == x) || z) False

-- | 4. map apply a given function elementwise to a list,
-- store results in new list.
map' :: (a -> b) -> [a] -> [b]
map' f x = foldr (\y ys -> (f y) :  ys) [] x

-- | 5. (++) using an anonymous function within foldr we append
-- the elements of list2 to list1 until list2 does not have elements anymore.
plusplus :: [a] -> [a] -> [a]
plusplus l2 l1 = foldr (\x xs -> x : xs) l1 l2

-- | 6. reverse (with foldr) using the previously definded plusplus
-- we add the element of the list  to an empty list, but in opposite order.
reverseR :: [a] -> [a]
reverseR a = foldr (\x z -> plusplus z [x]) [] a

-- |7. reverse (with foldl) with foldl we can just build a list as normal,
-- because foldl is in opposite order of foldr.
reverseL :: [a] -> [a]
reverseL a = foldl (\z x -> x : z) [] a

-- |8. (!!) (with foldl)
-- Below might work, but does not use foldl (tested, but still errors)
-- (!!) :: [a] -> Int -> a
-- (!!) 0 (x : _)  = x
-- (!!) i (_ : xs) = (Puzzles.!!) (i - 1) xs


-- |9. isPalindrome an empty list is always a palindrome.
-- to find a palindrome we checking whether its reverse is the same.
isPalindrome :: Eq b => [b] -> Bool
isPalindrome [] = True
isPalindrome b = (b == reverseL b)

-- |10. fibonnaci to build an infinite list, we start from 0, and with a list
-- with 1 in it. using a recursive definition we can create
-- the next elements of this list.
fibonacci = scanl (+) 0 (1: fibonacci)

-- |11. type List = (Int) -> Int
