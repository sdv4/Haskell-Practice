-- function to return the largest of three input integers. For use in the
-- following question.
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

-- 4.1: give three definitions of the function maxFour, which returns the
-- largest of four input numbers.
-- First function modeled on maxThree
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d = max (max (max a b) c) d

-- Second function modeled on original version of maxThree; w/o using max
maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d
 | a >= b && a >= c && a >= d     = a
 | b >= c && b >= d               = b
 | c >= d                         = c
 | otherwise                      = d

-- Third function should use max and maxThree
maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d = max (maxThree a b c) d
 --maxFour3 a b c d = maxThree a b (max c d)

-- 4.2 Function that True if the a <= b <= c
between :: Integer -> Integer -> Integer -> Bool
between a b c = a <= b && b <= c

-- Helper function for 4.2 between function. This return True when the sequence
-- m n p does not decrease at any point from left to right
-- weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
-- weakAscendingOrder m n p = m <= n && n <= p

-- 4.3 function to return the number of its three arguments that are equal
-- to another of the arguments
howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual a b c
 | a == b && b == c               = 3
 | a == b || a == c || b == c     = 2
 | otherwise                      = 0

-- 4.4 function to return the number of its four arguments that are equal
-- to another of the arguments. Needs to consider the case where a = b != c = d
-- which should return 4
howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual a b c d
 | a == b && c == d || a == d && b == c              = 4
 | howManyEqual a b c == 3 || howManyEqual a b d == 3 || howManyEqual a c d == 3  = 3
 | howManyEqual b c d == 3        = 3
 | a == b || a == c || a == d     = 2
 | b == c || b == d || c == d     = 2
 | otherwise                      = 0


-- 4.9 function to return the max of three integers paired with the number of
-- times it occurs among the three
maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c
 = (theMax, occurs)
   where
   theMax = max (max a b) c
   occurs = numOccur theMax (a, b, c)

-- Helper function for 4.9 counting the number of times a occurs in (x,y,z)
numOccur :: Int -> (Int, Int, Int) -> Int
numOccur a (x, y, z)
 | a == x && a == y && a == z       = 3
 | a == x && a == y || a == x && a == z || a == y && a == z       = 2
 | otherwise                        = 1

-- 4.17 a function which when given natural numbers m and n returns the product
-- of m and n and all integers in between.

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
 | m < 0 || n < 0       = error "rangeProduct requires natural number inputs"
 | n < m                = 0
 | n == m               = m
 | n > m                = (rangeProduct (m) (n-1)) * n


-- 4.18 A definition of fac that uses rangeProduct
frac :: Integer -> Integer
frac n = rangeProduct 1 n

-- 4.19 recursive function for multiplication using addition
recursMult :: Integer -> Integer -> Integer
recursMult x y
 | x == 0 || y == 0       = error "x and y must be natural numbers - greater than 0"
 | y == 1                 = x
 | y > 1                  = x + recursMult x (y-1)

-- TODO:::::::::: 4.20 function for INTEGER square root of n: the largest integer whose square
-- is less than or equal to n
intSqrt :: Float -> Float
intSqrt n
 | n < 0.0                    = error "Can't take sqrt of negative numbers"
 | n == 0.0                   = 0.0
 | n == 1.0                   = 1.0
-- | n > 1 && toInteger (sqrt n * sqrt n) == toInteger n   =  sqrt n
 | otherwise                = intSqrt (n-1)

-- TODO:::::::::: 4.21 given a function f of type (Integer -> Integer) and natural numnber n,
-- returns the max of f from n to 0, using recursion
returnMaxOfF :: (Integer -> Integer) -> Integer -> Integer
returnMaxOfF f n
 | n == 0             = f n
 | n == 1             = max (f 1) (f 0)
-- | n > 1              = max (returnMaxOfF f (n-1)) (returnMaxOfF f (n-2))

-- 4.22 given function f and input for f, n, returns True if one or more of the
-- values from 0 - n, when passed into f, returns 0; False otherwise
funReturnsZero :: (Integer -> Integer) -> Integer -> Bool
funReturnsZero f n
 | f 0 /= 0       = False
 | f 0 == 0       = True
 | f n == 0       = True
 | n > 0          = funReturnsZero f (n-1)


-- helper function f, for 4.22, with hard coded values
f :: Integer -> Integer
f n
 | n == 0     = 1
 | n == 1     = 44
 | n == 2     = 17
 | n == 3     = 70
 | otherwise  = 100

-- 4.23

-- 4.32
