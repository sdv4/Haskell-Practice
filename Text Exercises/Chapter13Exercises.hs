--13.1 my polymorphic function for evaluating not equals: /=
-- Using ==
notEqual :: Eq a => a -> a -> Bool
notEqual x y
 | x == y     = False
 | otherwise  = True

--13.2 function that takes a list of items and a potential element and returns
-- the number of times that the item occurs in the list.
numEqual :: Eq a => a -> [a] -> Int
numEqual x []     = 0
numEqual x (y:ys)
 | x == y         = 1 + numEqual x ys
 | otherwise      = numEqual x ys

-- function to check if an input x is an element of intput list, using numEqual
myMember :: Eq a => a -> [a] -> Bool
myMember x list = ((numEqual x list) > 0)

--13.3 function that takes a list of pairs and an item and returns the second part
-- of the first pair whose first element matches the item argument.
-- ex: oneLookupFirst [(1,3), (4,4), (9,2)] 9 returns 2
oneLookupFirst :: Eq a => [ (a,b) ] -> a -> b
oneLookupFirst [] x       = error "item not found as first element of list pair"
oneLookupFirst (x:xs) y
 | fst x == y             = snd x
 | otherwise              = oneLookupFirst xs y

oneLookupSecond :: Eq b => [ (a,b) ] -> b -> a
oneLookupSecond [] x     = error "item not found as second element of pair in list"
oneLookupSecond (x:xs) y
 | snd x == y            = fst x
 | otherwise             = oneLookupSecond xs y

--13.4
--type Triple = ()

--instance (Info a,Info b, Info c) => Info (a,b,c) where
  --    examples (a,b,c) = [(a,b,c)]

--13.7 Ord
--13.8
--class Eq a => MyOrd a where
--  (<), (<=), (>), (>=)  :: a -> a -> Bool
--  max, min              :: a -> a -> a
--  compare               :: a -> a -> Ordering
--  x < y                 = (x /= y && abs (y-x) == y-x)
--  x <= y                = (x < y || x == y)
--  x > y                 = (y < x)
--  x >= y                = (x > y || x == y)


--13.9
--13.11
