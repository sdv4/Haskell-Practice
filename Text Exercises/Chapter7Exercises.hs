module Chapter7Exercises
    (
    ) where
-- import Test.QuickCheck


-- Chapter 7 exercises
-- 7.1 pattern matching def of a function which returns first int of a list + 1
-- if the list contains at least one int and returns 0 otherwise
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:_) = x+1

-- 7.2 pattern matching def of a function that adds together the first two
-- Integers in a list, if it contains at least two elements; returns the head
-- element if the list is a singleton; returns 0 otherwise
sumFirstTwoOfList :: [Integer] -> Integer
sumFirstTwoOfList [] = 0
sumFirstTwoOfList [x] = x
sumFirstTwoOfList (x:(y:ys)) = x+y

-- 7.3 replicate the previous two functions using built-in functions, i.e.
--without pattern matching
headPlusOne2 :: [Int] -> Int
headPlusOne2 x
 | length x > 0 = (head x) + 1
 | otherwise    = 0

sumFirstTwoOfList2 :: [Integer] -> Integer
sumFirstTwoOfList2 x
 | length x == 0      = 0
 | length x == 1      = head x
 | otherwise          = sum (take 2 x)

-- 7.5 function which gives the product of a list of Integers and returns 1 for
-- an empty list. 1 was chosen for the empty list type because 1 is the unit
-- in multiplication. We begin multiplying the contents when we get to the empty
-- list. If it were 0, the result would always be zero.
myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * (product xs)

-- 7.6 function which gives the conjunction and disjunction of a list of
-- Boolean values. i.e. the result of a logical AND between each term
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && (and xs)

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || (myOr xs)

-- 7.8

-- 7.9
-- 7.11
-- 7.13
-- 7.16
-- 7.18
-- 7.19
-- 7.20
-- 7.21
-- 7.23
-- 7.24
-- 7.25
-- 7.27
-- 7.28
-- 7.29
-- 7.32
-- 7.33
-- 7.34
