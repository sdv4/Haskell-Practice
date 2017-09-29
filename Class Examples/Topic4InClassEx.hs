-- Topic 4 in-class examples

-- Take cross product of two 3-tuples
cross :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
cross (x1, y1, z1) (x2, y2, z2) = (y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2)
-- could also do: cross a b   but this isn't useful here

dot :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

-- Compute length of a vector
len :: (Double, Double, Double) - > Double
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
mySum []  = 0
-- mySum values = head values + (mySum (tail values))
mySum (v:vs) = v + mySum vs -- head of list in v and tail in vs;

-- ex 7: Run Length Encoding
decode :: [(Char, Int)] -> String
decode [] = ""                    -- base case for encoding
decode (p:ps) = (replicate (snd p) (fst p)) ++ decode ps  -- replicates the head element

-- exsercise: write encoder for this 
