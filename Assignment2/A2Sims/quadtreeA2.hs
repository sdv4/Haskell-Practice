--
-- CPSC 449 - Fall 2017
-- Assignment 2
-- Starter code authored by Professor Ben Stephenson
-- remaining code author: Shane Sims
-- version: 13 November 2017
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

--
-- Define your algebraic types for Part 1 here
--
-- NilTree (R,G,B)
data QuadTree = NilTree (Int, Int, Int) |                                       -- leaf node
                Node QuadTree QuadTree QuadTree QuadTree                        -- internal node with four children

-- Image sideLength colour
data Image = Image Int QuadTree

instance Show Image where
  show (Image size _) = "I am " ++ show(size) ++ " pixes wide and tall"

--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int
make32Int a b c d = ((((fromIntegral a) * 256) +
                       (fromIntegral b) * 256) +
                       (fromIntegral c) * 256) +
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block.  Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
-- Insert your code here for Parts 2, 3 and 4 here
--

-- Function to check if an image is homogoneous; that is, do all pixels have the
-- same colour.
isHomog :: [[(Int,Int,Int)]] -> Bool
isHomog img
 | ((filter (/=(head img)) img == []) && (filter (/= ((head(head img)))) (head img) == [])) = True -- first check if all rows are the same, then if all columns in a row are the same
 | otherwise                        = False

-- Function taking a list representation of an image and stores it in a quad tree
-- image (type Image)
createTree :: [[(Int,Int,Int)]] -> Image
createTree img
 | length(img) /= length(head img) = error "Input image must be a square"
 | isHomog img      = Image lenOfSide (NilTree (head(head img)))                -- If image is homogenous, make leafnode of the colour (i.e. of any of the pixel colours)
 | otherwise        = Image lenOfSide (Node tree1 tree2 tree3 tree4)            -- otherwise, break image into 4 smaller subimages; cont. until homogenous sub images result
   where
     topHalf        = fst (splitAt ((length img) `div` 2) img)                  -- split image into two - save top
     bottomHalf     = snd (splitAt ((length img) `div` 2) img)                  -- split image into two - save bottom
     -- take front and back half of each of topHalf and bottomHalf
     subLength      = (length (head topHalf)) `div` 2                           -- Length of a row/2
     subImg1        = [ take subLength x | x <- topHalf]                        -- first quadrent of non-homogenous image
     subImg2        = [ drop subLength x | x <- topHalf]                        -- second quadrent of non-homogenous image
     subImg3        = [ take subLength x | x <- bottomHalf]                     -- third quadrent of non-homogenous image
     subImg4        = [ drop subLength x | x <- bottomHalf]                     -- fourth quadrent of non-homogenous image
     (Image size1 tree1) = createTree subImg1                                   -- call createTree recursively to get tree for subimage1
     (Image size2 tree2) = createTree subImg2                                   -- call createTree recursively to get tree for subimage2
     (Image size3 tree3) = createTree subImg3                                   -- call createTree recursively to get tree for subimage3
     (Image size4 tree4) = createTree subImg4                                   -- call createTree recursively to get tree for subimage4
     lenOfSide = (length (head img))

-- Map function for QuadTree. Maps input function over all nodes in a QuadTree,
-- returning the modified result
--
-- Input 1: Function to map over nodes of input QuadTree
-- Input 2: QuadTree to be modified
-- Output: Modified QuadTree
treeMap :: (QuadTree -> QuadTree) -> QuadTree -> QuadTree
treeMap f (NilTree colour)   = f (NilTree colour)
treeMap f (Node t1 t2 t3 t4) = f (Node (treeMap f t1) (treeMap f t2) (treeMap f t3) (treeMap f t4))

greyscale :: Image -> Image
greyscale (Image size tree) = (Image size (treeMap greyMe tree))

greyMe :: QuadTree -> QuadTree
greyMe (NilTree (r,g,b)) = (NilTree (ave, ave, ave))
 where
   ave = (r+g+b) `div` 3
greyMe (Node t1 t2 t3 t4) = (Node (greyMe t1) (greyMe t2) (greyMe t3) (greyMe t4))

mirror :: Image -> Image
mirror (Image size tree) = (Image size (treeMap mirrorMe tree))


mirrorMe :: QuadTree -> QuadTree
mirrorMe (Node t1 t2 t3 t4) = (Node t2 t1 t4 t3)
mirrorMe (NilTree (r,g,b))   = (NilTree (r,g,b))


-- Function that draws a rectangle, specified by paramaters
-- Paramaters:
--   x, y: The upper left corner of rectangle
--   w, h: The width and height of rectangle
-- r, g, b: The red, green, blue colour codes (respectively), for fill colour
--
-- Returns:
-- A string for an SVG rect shape.
--note: from code authored for assignment 1
drawRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
drawRectangle x y w h r g b = "<rect x=" ++ (show x) ++
                              " y=" ++ (show y) ++
                              " width=" ++ (show w) ++
                              " height=" ++ (show h) ++
                              " stroke=\"black\"" ++
                              " stroke-width=\"0\"" ++
                              " fill=\"rgb(" ++ (show r) ++ "," ++
                                                (show g) ++ "," ++
                                                (show b) ++ ")\" />\n"

-- Function for traversing the nodes of a quad tree
-- Paramaters:
-- x, y: the upper left corner of the image
-- image: the quad tree to representing the image to be rendered
--
-- Returns: a string representing the html code required to render the image.
htmlHelper :: Int -> Int -> Image -> String
htmlHelper x y (Image size (NilTree (r,g,b))) = drawRectangle x y size size r g b
htmlHelper x y (Image size (Node t1 t2 t3 t4))= topL ++ bottomL ++ topR ++ bottomR
 where
  halfSize = size `div` 2
  topL = htmlHelper x y (Image halfSize t1)
  topR = htmlHelper x (y + halfSize) (Image halfSize t3)
  bottomL = htmlHelper (x + halfSize) y (Image halfSize t2)
  bottomR = htmlHelper (x + halfSize) (y + halfSize) (Image halfSize t4)


-- Function to generate the HTML SVG tags required to render a quad tree image (ie.
-- an Image object) in a browser, by recursively generating the svg tags for the
-- image by calling htmlHelper, and attaching a html prefix and suffix to the
-- recursively genreated string.
--
-- Parameters:
-- Input: Image to be rendered in HTML
-- Output: String of HTML code
toHTML :: Image -> String
toHTML (Image size tree) = "<html><head></head><body bgcolor=\"#FFFFFF\">\n" ++
             "<svg width=\"" ++ (show size) ++
             "\" height=\"" ++ (show size) ++ "\">" ++
            (htmlHelper 0 0 (Image size tree)) ++ "</svg>\n</html>"





--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  input <- BS.readFile "ThisSideUp.png"

  -- image is the list representation of the image stored in the .png file
  let image = decodeImage input

  -- Convert the list representation of the image into a tree representation
  let qtree_image = createTree image

  -- Gray scale the tree representation of the image
  let gs = greyscale qtree_image

  -- Mirror the tree representation of the image
  let mi = mirror qtree_image

  -- Write the original, mirrored and grayscale images to quadtree.html
  --writeFile "Mondrian.html" ((toHTML qtree_image) ++ "<br><br><br>") -- take this line out and use the lines below
                               -- instead once you have your functions written

  writeFile "quadtree2.html" ((toHTML qtree_image) ++ "<br><br><br>" ++
                             (toHTML gs) ++ "<br><br><br>"
                            ++ (toHTML mi) ++ "<br><br><br>")
