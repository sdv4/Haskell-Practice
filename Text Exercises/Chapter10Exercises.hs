-- Exercises from Chapter 10

--10.1 Three different doubleAll functions, using: list comprehension, primitive
-- recursion, and map
doubleAll1 :: [Int] -> [Int]
doubleAll1 list     = [e*2 | e <- list]

doubleAll2 :: [Int] -> [Int]
doubleAll2 []       = []
doubleAll2 [x]      = [2*x]
doubleAll2 (x:xs)   = 2*x : doubleAll2 xs

double :: Int -> Int
double x    = 2*x

doubleAll3 :: [Int] -> [Int]
doubleAll3 xs     = map double xs

--10.2 define a length function using map and sum
--myLength :: [a] -> Int


-- helpers for 10.3
-- Funcition returning True if the passed in int is greater than 1; else False
greaterOne :: Int -> Bool
greaterOne n = n > 1

-- Function which takes an int and returns the value with one added to it
addOne :: Int -> Int
addOne n = n + 1

--10.3 given the following funcition:
-- which adds one to every element of the list and then filters those that are
-- not greater than one - effectively filtering those <= zero before the map was applied
addUp :: [Int] -> [Int]
addUp ns = filter greaterOne (map addOne ns)

-- how would you redefine it using filter before map
-- we should add one to each element and then filter those that are not greater
-- than 0
greaterZero :: Int -> Bool
greaterZero n = n > 0

-- addUp2 [1,2,1,0,1] should return [2,3,2,2]
addUp2 :: [Int] -> [Int]
addUp2 ns = map addOne (filter greaterZero ns)


--10.4 the effect of map addOne (map addOne ns) is to add 1 to each element of ns
-- and then pass this list to another mapping of addOne, were 1 is again added to each element
-- the effect begin that 2 is added to each element of the list ns.
-- In general map f (map g xs) will have the result of mapping g over the elements
-- of xs, and the results will then have f mapped over them


--10.5 the effect of filter greaterOne (filter lessTen ns) is to filter all numbers
-- less than 10 into a list and then from this list filter all of those greater than
-- one into a final list. So the result is a list of numbers in [2,9]

--10.6 Function to take a list of integers ns and return the list consisting of
-- squares of the integers in a list
square :: Integer -> Integer
square x = x*x

squareWithMap :: [Integer] -> [Integer]
squareWithMap ns = map square ns

-- function that returns the sum of squares of items in ns
sumOfSquaresOfItems :: [Integer] -> Integer
sumOfSquaresOfItems ns = sum (squareWithMap ns)

-- function checking if all items of a list are greater than zero
allGreaterZero :: [Int] -> Bool
allGreaterZero ns = (filter greaterZero ns) == ns

--10.7 using already defined functions whenever possible, write the following:
-- function to give the minimum value of a function f on inputs 0 to n
minOfValsMapped :: (Int -> Int) -> Int -> Int
minOfValsMapped f n = minimum (map f [0..n])

-- function to test if all falues of f on 0 to n are equal

-- helper
allEqualElements :: [Int] -> Bool
allEqualElements []     = True
allEqualElements [x]    = True
allEqualElements [x,y]  = x == y
allEqualElements (x:y:ys)
 | x == y               = allEqualElements (y:ys)
 | otherwise            = False

allEqualOnF :: (Int -> Int) -> Int -> Bool
allEqualOnF f n = allEqualElements (map f [0..n])

-- Function to test if all values of f on inputs 0 to n are all greater than 0
allGreaterZeroOnF :: (Int -> Int) -> Int -> Bool
allGreaterZeroOnF f n = allGreaterZero (map f [0..n])

--Function to check if the values of f 0, f 1, f 2 are in increasing order
allThreeIncreasing :: (Int -> Int) -> Bool
allThreeIncreasing f = (f 0) < (f 1) && (f 1) < (f 2)

--10.8 function that applies twice a function f on input n
twice :: (Int -> Int) -> Int -> Int
twice f n = f (f n)

--10.9
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x            = x
iter 1 f x            = f x
iter n f x = f (iter (n-1) f x)


--10.10 function using iter and double that returns 2^n on input n
twoToPowerOfn :: Int -> Int
twoToPowerOfn n = iter (n-1) double 2

--10.13
--10.14
--10.15
--10.16
--10.17
--10.18
--10.19
--10.20
--10.21
--10.24
--10.25
--10.26
