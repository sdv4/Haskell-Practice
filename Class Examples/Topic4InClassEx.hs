-- Topic 4 in-class examples

-- Take cross product of two 3-tuples
cross :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
cross (x1, y1, z1) (x2, y2, z2) = (y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2)
-- could also do: cross a b   but this isn't useful here

dot :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

-- Compute length of a vector
len :: (Double, Double, Double) -> Double
len (x,y,z) = sqrt(x*x + y*y + z*z)

-- Example 4: function returns True if list is empty and false otherwise
isEmptyList :: [Int] -> Bool
isEmptyList [] = True
isEmptyList _  = False

-- alt for ex 4
isEmptyList2 :: [Int] -> Bool
isEmptyList2 x
 | length x == 0    = True
 | otherwise        = False

-- ex 5:   TODO: what is going on here?
isSingletonList :: [Int] -> Bool
isSingletonList (x:[])  = True
isSingletonList _       = False

-- ex 6: function to sum all elements of a list, using pattern matching
mySum :: [Int] -> Int
mySum []  = 0 -- base case: empty list
-- mySum values = head values + (mySum (tail values))
mySum (v:vs) = v + mySum vs -- take the from ele of list, name it v, take the remaining els and make them vs

-- ex 7: Run Length Encoding
decode :: [(Char, Int)] -> String
decode [] = ""                    -- base case for encoding
-- decode (p:ps) = (replicate (snd p) (fst p)) ++ decode ps  -- replicates the head element
decode ((char,count):tailOfList) = (replicate count char) ++ decode tailOfList

-- function to return the number of identical characters at the beginning of
-- the string
prefixCount :: String -> Int
prefixCount []      = 0
prefixCount (_:[])  = 1                     --(_:[]) is the pattern we are trying to match. this is a char array with one char followed by no others i.e. the single char string
prefixCount (c1:c2:cs)
 | c1 == c2        = 1 + prefixCount (c2:cs)
 | otherwise       = 1                     -- i.e. we have a string of length 2


-- exsercise: write encoder for this
encode :: String -> [(Char, Int)]
encode ""           = []
ensode s            = ((head s, prefixCount s) : encode (drop (prefixCount s) s))


-- List Comprehensions

-- to remove all odd values in a list and return the result
removeOdd :: [Int] -> [Int]
removeOdd values = [val | val <- values, val `rem` 2 == 0]

-- convert list of celcius temperatures to farenheit
celciusToFarenheit :: [Float] -> [Float]
celciusToFarenheit celc = [ (c * (9.0/5.0)) + 35.0 | c <- celc]

-- square all of the even ints in list and discard all odds
squareEvenDropOdd :: [Int] -> [Int]
squareEvenDropOdd orig = [ x*x | x <- orig, x `rem` 2 == 0]
