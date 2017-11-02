import Data.Tuple


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

-- a text example that checks if an element is in a list
myElem :: Integer -> [Integer] -> Bool
myElem x []     = False
myElem x (y:ys) = (x == y) || myElem x ys

-- 7.8 function that returns the number of times an element occurs in a list
-- version using primitive recursion
elemNumRecursive :: Integer -> [Integer] -> Integer
elemNumRecursive x []     = 0
elemNumRecursive x (y:ys)
 | x == y                 = 1 + elemNumRecursive x ys
 | otherwise              = elemNumRecursive x ys

-- non recursive version
elemNum :: Integer -> [Integer] -> Integer
elemNum x l = fromIntegral (length [ el | el <- l, el == x])


-- Helper function for 7.9 which removes all of a specified elementx from
-- a list
removeElems :: Integer -> [Integer] -> [Integer]
removeElems x []      = []
removeElems x (y:ys)
 | x == y             = removeElems x ys
 | otherwise          = y : removeElems x ys

-- 7.9 function that returns a list of values that occur only once in a
-- given list
-- ex: unique [1,2,3,4,1,2] returns [1,2,3,4]
unique :: [Integer] -> [Integer]
uinque []       = []
unique [x]      = [x]
unique (x:xs)
 | myElem x xs  = unique (removeElems x xs)
 | otherwise    = x : unique xs

-- 7.11 definitions of the prelude function unzip and reverse using primitive
-- recursion
myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = (myReverse xs) ++ [x]

--myUnzip takes a list of 2-tuples (pairs) and returns a pair of lists
-- lists. ex myUnzip [(3,'bob'), (5,'ned'), (1,'shirly')] returns ([3,5,1], ['bob','ned','shirly'])
--myUnzip :: [(a,b)] -> ([a],[b])
--myUnzip []          = ([],[])
--myUnzip [x]         = ([fst x],[snd x])
--myUnzip [x:y:ys]    =

-- 7.13
-- 7.16
-- Helper functions for 7.16
-- function to insert a given int into a lish of ints. Inserts the given x
-- element in front of the first element <= x found in the list
ins :: Integer -> [Integer] -> [Integer]
ins x []    = [x]
ins x (y:ys)
 | x <= y     = x:(y:ys)
 | otherwise  = y : (ins x ys)

iSort :: [Integer] -> [Integer]
iSort []          = []
iSort (x:xs)      = ins x (iSort xs)

-- insert x infront of the first element in the list that is
-- in descending order
insAlt :: Integer -> [Integer] -> [Integer]
insAlt x []    = [x]
insAlt x (y:ys)
 | x >= y     = y:(x:ys)
 | otherwise  = y : (insAlt x ys)

iSort2 :: [Integer] -> [Integer]
iSort2 []          = []
iSort2 (x:xs)      = insAlt x (iSort2 xs)

-- 7.19 Using modified versions of ins and iSort functions, this function sorts a
-- list of pairs of numbers in lexicographic order, by looking first at the first number
-- and only looking at the second if the first numbers are equal.
insPair :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
insPair p []      = [p]
insPair p (y:ys)
 | (fst p) < (fst y)                        = p : (y:ys)
 | (fst p) == (fst y) && (snd p < snd y)    = p : (y:ys)
 | otherwise                                = y : (insPair p ys)

iSortPair :: [(Int,Int)] -> [(Int,Int)]
iSortPair []      = []
iSortPair (x:xs)  = insPair x (iSortPair xs)



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
