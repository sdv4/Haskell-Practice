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


-- 5.15

-- 5.16

-- 5.17

-- 5.18

-- 5.19

-- 5.20

-- 5.21

-- 5.22

-- 5.23

-- 5.24

-- 5.26

-- 5.28
