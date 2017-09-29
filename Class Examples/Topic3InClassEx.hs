-- Recursive Fibonacci function with O(n^2) runtime
recursiveFib :: Integer -> Integer
recursiveFib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = recursiveFib (n-1) + recursiveFib (n-2)


-- Iterative Fibonacci function with while loop and O(n) runtime
iterativeFib :: Integer -> Integer
iterativeFib n
 | n == 0     = 0
 | n == 1     = 1
 | n < 0      = error "i'th Fibonacci number must be positive integer"
 | otherwise  = iFibH n 2 1 0

 -- ?
iFibH :: Integer -> Integer -> Integer -> Integer -> Integer
iFibH n i fm1 fm2
 | i < n      = iFibH n (i+1) (fm1+fm2) fm1
 | otherwise  = fm1 + fm2





 --
 --  Return the tag for a square in the middle of a region
 --
square :: Int -> Int -> Int -> Int -> String
square x y w h =
 "<rect x=" ++ (show (x + div w 4)) ++
 " y=" ++ (show (y + div h 4)) ++
 " width=" ++ (show (div w 2)) ++
 " height=" ++ (show (div h 2)) ++
 " fill=\"black\" stroke=\"none\" />"

 --
 --  tsquare: Identify the tags needed to draw the fractal tsquare
 --  Parameters:
 --    x, y: The top left corner of the current region
 --    w, h: The width and height of the region
 --  Return:
 --    A string containing tags that can be rendered within an HTML doc
tsquare :: Int -> Int -> Int -> Int -> String
tsquare x y w h
 | w >= 8 && h >= 8 = -- draw a square in the middle of the region
                      (square x y w h) ++
                      -- call myself recursively 4 times
                      (tsquare x y (w `div` 2) (h `div` 2)) ++
                  --    (tsquare (x + (w  `div` 2)) y (w 'div' 2) (h `div` 2)) ++
                  --    (tsquare x (y + (h `div` 2)) (w `div` 2) (h `div` 2)) ++
                  --  (tsquare (x + (w `div` 2) (y + (h `div` 2))   )
 | otherwise        = square x y w h

main :: IO ()
main = do
 let prefix = "<html><svg width=\"512\" height=\"512\">"
 let suffix = "</svg></html>"
 let image_tags = tsquare 0 0 512 512

 writeFile "tsquare.html" (prefix ++ image_tags ++ suffix)




-- example to show mutual recursion
isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n-1)


isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n-1)
