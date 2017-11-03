
--11.7 function returning true for non whitespace characters, i.e. Chars not in
-- " \n\t"
notWhiteSpace :: Char -> Bool
notWhiteSpace c = (\x -> not(elem x " \n\t")) c

--11.8 function that given function f, returns the function that when given n gives
-- the total of f 0 + f 1 + ... + f n
total :: (Integer -> Integer) -> (Integer -> Integer)
total f = (\x -> foldr1 (+) (map f [0..x]))

--11.9 a lambda expression that takes a function taking two arguments and returns
-- a function with the argument order flipped: (\f a b -> f b a)

--11.10
switchArgs :: (a -> b -> c) -> (b -> a -> c)
switchArgs fun = (\f a b -> f b a) fun

myMultiply :: Int -> Int -> Int
myMultiply x y = x*y
doubleAll :: [Int] -> [Int]
doubleAll = map (myMultiply 2)

--11.11
--comp2∷

--total2 :: (Integer -> Integer) -> (Integer -> Integer)
--total2 f = foldr1 (+) . map f [1..n]

--11.12 map (+1) . filter (>(-1))


--helpers for following questions - from text pg.248
double :: Integer -> Integer
double n = 2 * n

iter :: Integer -> (a -> a) -> (a -> a)
iter n f
 | n > 0        = f . iter (n - 1) f
 | otherwise    = id

--11.19 iter 3 double 1 = 8

--11.20
--type: Integer -> (Integer -> Integer)
--adds one to the input value n times

--11.21 alternate and constructive definition of iter which creates the list of
-- n copies of f
constIter :: Int -> (a -> a) -> (a -> a)
constIter n f        = foldr1 (.) (replicate n f)
