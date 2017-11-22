--
-- CPSC 449 Assignment 1
-- Author: Shane Sims
-- Version: 17 October 2017
-- Generate and output a Mondrian-style image as an SVG tag within an HTML
-- document, according to the algorithm given in the assignment description.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
--
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

-- Function that draws a rectangle, specified by paramaters
-- Paramaters:
--   x, y: The upper left corner of rectangle
--   w, h: The width and height of rectangle
-- r, g, b: The red, green, blue colour codes (respectively), for fill colour
--
-- Returns:
-- A string for an SVG rect shape.
--
drawRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
drawRectangle x y w h r g b = "<rect x=" ++ (show x) ++
                              " y=" ++ (show y) ++
                              " width=" ++ (show w) ++
                              " height=" ++ (show h) ++
                              " stroke=\"black\"" ++
                              " stroke-width=\"2\"" ++
                              " fill=\"rgb(" ++ (show r) ++ "," ++
                                                (show g) ++ "," ++
                                                (show b) ++ ")\" />\n"

-- Generate SVG tag for a rectangle with random color (from choice of 4).  R
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:vs:hs:rest)
 | w > (width `div` 2) && h > (height `div` 2)  =                                --if region > half height and > half width ; then split into 4 randomly
                                                  (bottom_right_rest, top_left_string ++
                                                  bottom_left_string ++
                                                  top_right_string ++
                                                  bottom_right_string)
 | w > (width `div` 2)                          =                               --else if only region > half width ; then split into 2 randomly, vertically
                                                  (right_rest, left_string ++
                                                  right_string)
 | h > (height `div` 2)                         =
                                                  (bottom_rest, top_string ++   --else if only region > half height ; then split into 2 randomly, horizontally
                                                  bottom_string)
 | wide_enough && tall_enough                   =                               -- else if wide enough and tall enough, decide if and where to split into 4, randomly
                                                  (bottom_right_rest,
                                                  top_left_string ++
                                                  bottom_left_string ++
                                                  top_right_string ++
                                                  bottom_right_string)
 | wide_enough                                  =                               -- else if only wide enough, decide if and where to split vertically, randomly
                                                  (right_rest, left_string ++
                                                  right_string)
 | tall_enough                                  =                               -- else if only tall enough, decide if and where to split horizontally, randomly
                                                  (bottom_rest, top_string ++
                                                  bottom_string)
                                                                                -- Else, fill with colours or white, using randomness
 | r < 0.0833                     = (rest, drawRectangle x y w h 255 0 0)       -- Fill shape red

 | r < 0.1667                     = (rest, drawRectangle x y w h 135 206 235)   -- Fill shape sky blue
 | r < 0.25                       = (rest, drawRectangle x y w h 255 255 51)    -- Fill shape yellow
 | otherwise                      = (rest, drawRectangle x y w h 255 255 255)   -- Fill shape with white; randomly decided

 where
    w_mult = (round((fromIntegral w) * 1.5))                                    -- for use in wide_enough
    h_mult = (round((fromIntegral h) * 1.5))                                    -- for use in tall_enough

    wide_enough = w_mult > 120 && (randomInt 120 w_mult vs) < w                 -- True if w is wide enough to split; False otherwise
    tall_enough = h_mult > 120 && (randomInt 120 h_mult hs) < h                 -- True if h is tall enough to split; False otherwise

    -- Determine vertical and horizontal split points randomly
    x_bar = round((fromIntegral w) * ((fromIntegral (randomInt 33 66 vs))/100.0))-- The vertical point where a split begins
    y_bar = round((fromIntegral h) * ((fromIntegral (randomInt 33 66 hs))/100.0))-- The horizontal point where a split begins

    -- For use when splitting a region into four
    (top_left_rest, top_left_string)         = mondrian x y x_bar y_bar (rest)                                                   --
    (bottom_left_rest, bottom_left_string)   = mondrian x (y + y_bar) x_bar (h - y_bar) top_left_rest
    (top_right_rest, top_right_string)       = mondrian (x + x_bar) y (w - x_bar) y_bar bottom_left_rest
    (bottom_right_rest, bottom_right_string) = mondrian (x + x_bar) (y + y_bar) (w - x_bar) (h - y_bar) top_right_rest

    -- For use when spliting a region with vertical line only
    (left_rest, left_string)                 = mondrian x y x_bar h (rest)
    (right_rest, right_string)               = mondrian (x + x_bar) y (w - x_bar) h left_rest

    -- For use when splitting a region with a horizontal line only
    (top_rest, top_string)                   = mondrian x y w y_bar (rest)
    (bottom_rest, bottom_string)             = mondrian x (y + y_bar) w (h - y_bar) top_rest

-- The main program which generates and outputs mondrian.html.
main :: IO ()
main = do

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body bgcolor=\"#FFFFFF\">\n" ++
               "<svg width=\"" ++ (show width) ++
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
