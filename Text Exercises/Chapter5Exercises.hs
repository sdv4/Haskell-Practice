import Data.Char as Char

-- 5.1 function returning the max two integers along with the number
-- of times this Integer occurs in the pair
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
 | x > y                = (x, 1)
 | x < y                = (y, 1)
 | otherwise            = (x, 2)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs x y z
 | h1 == h2                                = (h1, o1+o2-1)
 | h1 > h2                                 = (h1, o1)
 | otherwise                               = (h2, o2)
 where
   h1 = fst (maxOccurs x y)
   h2 = fst (maxOccurs z h1)
   o1 = snd (maxOccurs x y)
   o2 = snd (maxOccurs z h1)


-- function to return the largest of three input integers. For use in the
-- following question.
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z
 | x <= y && x <= z = x
 | y <= z = y
 | otherwise = z

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
 | x <= y && x >= z || x <= z && x >= y      = x
 | y <= x && y >= z || y <= z && y >= x      = y
 | otherwise                                 = z


-- 5.2 function which puts elements of a triple into ascending order
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (minThree x y z, middle x y z, maxThree x y z)

-- 5.3 function to returns the x coordinate where a straight line crosses the
--  x-axis of a graph.
-- Line must not have slope 0. Input is of the form y = mx+b: (m,b)
xIntercept :: (Integer, Integer) -> Float
xIntercept (0,b) = error "slope can not be 0"
xIntercept (m,b) = (fromIntegral (-b))/(fromIntegral m)


-- 5.15 the expression [0, 0.1 .. 1] evaluates to [0.0,0.1,0.2,
-- 0.30000000000000004,0.4000000000000001,0.5000000000000001,0.6000000000000001,
-- 0.7000000000000001,0.8,0.9,1.0]. I thought it would evaluate to:
-- [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]. Maybe this difference
-- is a result of switching between evaluating Integer types and Float types?

-- 5.16 The list [2,3] contains two items, the Integers 2 and 3. The list
-- [[2,3]] contains one item; namely the list [2,3]

-- 5.17 The result of evaluating [2 .. 2] in ghci is [2]
-- The result of evaluating [2, 7 .. 4] is the list [2]. This is because the
-- step is calculated as 7 - 2 = 5, but stepping from 7 would bring us to 12,
-- which is already greater than the declared max element size of the last
-- list element. Similarly, 7 is higher than this max, so it is also ignored,
-- leaving the singleton, [2]
-- Finally, evaluating [2, 2 .. 2] results in a list of 2's as large as can be
-- held in memory. This is because the step is
-- calculated as 2-2=0, so we will increment 2 by 0 and since this incrementation
-- never leads to a value greater than 2, the list will keep growing accordingly.

-- 5.18 function to double all elements of a list and return result as a list
doubleAll :: [Integer] -> [Integer]
doubleAll listOfInts = [2*n | n <- listOfInts]

-- 5.19 function to take a String, i.e. list of Char, and capitalize small
-- letter, returning the result as a String
capitalize :: String -> String
capitalize theString = [Char.toUpper x | x <- theString]

-- This version removes any non letters while capitalizing the letters
capitalizeLetters :: String -> String
capitalizeLetters theString = [Char.toUpper x | x <- theString, isLetter x]

-- 5.20 function to return the list of divisiors of a positive input Integer or
-- an empty list if the input is something other than a String
divisors :: Integer -> [Integer]
divisors n = [ posibleDiv | posibleDiv <- [1 .. n], (n `mod ` posibleDiv) == 0]

isPrime :: Integer -> Bool
isPrime n
 | divisors n == [1,n]      = True
 | otherwise                = False

-- 5.21 function  which picks out all occurrences of an integer n in a list,
-- returning these integers as a list
matches :: Integer -> [Integer] -> [Integer]
matches n theList = [ i | i <- theList, i == n ]

-- function that uses matches to return True if the input Integer is an element
-- of the list and False otherwise
elemm :: Integer -> [Integer] -> Bool
elemm n theList
 | matches n theList == []  = False
 | otherwise                = True

-- 5.22 TODO: check solution. function which takes a list of Strings and returns a single String ,
-- which when printed, shows the strings on seperate lines
--onSeperateLines :: [String] -> String
--onSeperateLines st = [ str ++ ['\n'] | str <- st]

-- 5.23 function which takes a String and an Int and returns n copies of the
-- string as a String
duplicate :: String -> Integer -> String
duplicate a n
 | n == 0     = ""
 | n < 0      = ""
 | otherwise  = a ++ (duplicate a (n-1))

-- 5.24 TODO: how would we make line length a parameter of this funciton?
-- pushRight :: String -> String


-- TODO: figure out 5.22 before doing this
-- 5.26 a function which produces a table of fib numbers from 0 to input n
-- fibTable :: Integer -> String
