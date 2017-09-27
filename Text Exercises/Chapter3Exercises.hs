module Chapter3Exercises where

-- 3.1 function that works as: exclusive or of x and y will be True if
-- either x is True and y is False, or x is False and y is True.

altExOr :: Bool -> Bool -> Bool
altExOr True False = True
altExOr False True = True
altExOr True True = False
altExOr False False = False

-- 3.5 Two definitions of nAnd (not and) function, which
-- return the result True except when both args are True

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd True False = True
nAnd False True = True
nAnd False False = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 x y = not x&&y

-- 3.9 function to check if ALL three numbers m, n, p
-- are different

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = ((m /= n) && (m /= p) && (n /= p))

-- 3.10 function that uses threeEqual to check if four
-- numbers m, n, p, and q are all different

fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent m n p q = (threeDifferent m n p) && (threeDifferent m n q) && (threeDifferent m p q)


-- 3.11 Calculations of various expressions

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual x y z = (x == y) && (x == z) && (y == z)


-- threeEqual (2+3) 5 (11 `div` 2) = True


-- 3.13 functions to determine the max x, y, and z.
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z
 | x >= y && x >= z = x
 | y >= z = y
 | otherwise = z

-- max (3-2) (3*8) = 24
-- maxThree (4+5) (2*6) (100 `div` 7) = 14

-- 3.14 functions to return the minimum of x and y, or x, y, and z (resp.).
min' :: Integer -> Integer -> Integer
min' x y
 | x <= y = x
 | otherwise = y

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z
 | x <= y && x <= z = x
 | y <= z = y
 | otherwise = z

-- 3.16 function to convert small letters to capitals. Returns unchanged
-- characters which are not small letters to begin with. Helper function
-- determines if a given character is upper case to begin with.

isUpper :: Char -> Bool
isUpper ch = (fromEnum ch <= fromEnum 'Z') && (fromEnum ch >= fromEnum 'A')

offset :: Int
offset = (fromEnum 'A') - (fromEnum 'a') -- store offset in ASCII code from capital to small letters

toUpper :: Char -> Char
toUpper ch
 | isUpper ch  = ch
 | otherwise = toEnum (fromEnum ch + offset)

-- 3.17 function that converts a Char digit (ex. '8') to its Int value (ex. 8), for digits 0-9
-- Non-digits are taken to be '0'
numberOffset :: Int
numberOffset = 48

charToNum :: Char -> Int
charToNum ch
 | (fromEnum ch < 48) || (fromEnum ch > 57) = 0
 | otherwise = (fromEnum ch - 48)

-- 3.18 function that takes three strings and returns a single string, that
-- when printed, shows each string on a seperate line
onThreeLines :: String -> String -> String -> String
onThreeLines st1 st2 st3 = st1 ++ "\n" ++ st2 ++ "\n" ++ st3 ++ "\n"

-- 3.19 function takes a digit from 0 - 9 as a Char and returns a String
-- of the roman numeral of this number. Chars not in this interval are
-- taken to be '0'
romanDigit :: Char -> String
romanDigit ch
 | ch == '1' = "I"
 | ch == '2' = "II"
 | ch == '3' = "III"
 | ch == '4' = "IV"
 | ch == '5' = "V"
 | ch == '6' = "VI"
 | ch == '7' = "VII"
 | ch == '8' = "VIII"
 | ch == '9' = "IX"
 | otherwise = "nulla"


-- 3.20 function to return the average of three integers and a function to
-- determine how many of the input integers are above the average of all
-- of the input integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = (fromIntegral (x + y + z))/ 3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z
 | fromIntegral x > (averageThree x y z) && fromIntegral y > (averageThree x y z) = 2
 | fromIntegral x > (averageThree x y z) && fromIntegral z > (averageThree x y z) = 2
 | fromIntegral y > (averageThree x y z) && fromIntegral z > (averageThree x y z) = 2
 | fromIntegral x > (averageThree x y z) || fromIntegral y > (averageThree x y z) || fromIntegral z > (averageThree x y z) = 1
 | otherwise = 0

-- 3.22 function to count the number of non-degenerate roots of a quadratic
-- equation. Assumes that 'a' term in ax^2 + bx + c = 0 is non-zero
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
 | b^2 > (4.0*a*c) = 2
 | b^2 == (4.0*a*c) = 1
 | b^2 < (4.0*a*c) = 0



-- 3.23 function as above but includes the degenerate case produced when a = 0
-- but return value of 3 indicates that every real number a root
numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
 | a /= 0.0 = numberNDroots a b c
 | b /= 0.0 = 1
 | b == 0.0 && c /= 0.0 = 0
 | b == 0.0 && c == 0.0 = 3

-- 3.24 functions that return the smaller and larger roots (resp.)
-- of a given quadratic equation with coefficients a b c.
smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = ((negate b) - (sqrt (b^2 - (4*a*c))))/(2.0*a)
largerRoot a b c = ((negate b) + (sqrt (b^2 - (4*a*c))))/(2.0*a)
