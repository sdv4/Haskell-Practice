module FirstScript where

-- The value 'size' is an Integer, defined to be the 
-- sum of twelve and thirteen.

size :: Integer
size = 12+13

-- The function to square an integer

square :: Integer -> Integer
square n = n*n

-- The function to double an Integer
double :: Integer -> Integer
double n = 2*n

-- An example using size, square and double.

example :: Integer
example = double (size - square (2+2))

-- A function to double its input and square the result
-- of that operation.

doubleThenSquare :: Integer -> Integer
doubleThenSquare n = square (double n)

-- A funciton to square its input and then double
-- the result of that operation

squareThenDouble :: Integer -> Integer
squareThenDouble n = double (square n)
