--6.4 function that takes two chars (which chold be atoms of a Picture, so that
-- '.' is while and '#' is black) and puts them 'on top of' eachother.
superimposeChar :: Char -> Char -> Char
superimposeChar x y
 | x == '.' && y == '.'     = '.'
 | otherwise                = '#'

--6.5 faunciton that takes two lines (of a Picture element) superimposes their
-- corresponding characters using superimposeChar. Assume that line lengths are
-- the same
superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine lineFst lineSnd
  = [superimposeChar charFst charSnd|(charFst, charSnd) <- zip lineFst lineSnd]

-- def of Picture type for use in 6.6 and 6.7
type Picture =[[Char]]

--6.6 function which opeates as that in 6.5 to superimpose two Picture item
superimpose :: Picture -> Picture -> Picture
superimpose picOne picTwo
  = [superimposeLine linePicOne linePicTwo | (linePicOne, linePicTwo) <- zip picOne picTwo]


-- 5.22 TODO: check solution. function which takes a list of Strings and returns a single String ,
-- which when printed, shows the strings on seperate lines
onSeperateLines :: [String] -> String
onSeperateLines st = concat [ str ++ ['\n'] | str <- st]

--6.7 prints a picture by printing each [Char] of the Picture [[Char]] on a
-- seperate line in order from beginning to end
printPicture :: Picture -> IO ()
printPicture thePic = putStr (onSeperateLines thePic)


-- type definitions for exercises 6.29 to 6.31
-- a Position is defined as a location on the x-y plane
type Position = (Int, Int)
-- an Image is defined as a Picture with a Position, which is the location of
-- its bottom left corner (i.e. its reference point)
type Image = (Picture, Position)


--6.29 Image constructor
makeImage :: Picture -> Position -> Image
makeImage thePic thePosition = (thePic, thePosition)

--6.30 function that takes an Image and returns a new image with the same
-- Picture but whose new Position is that of the input parameter
changePosition :: Image -> Position -> Image
changePosition theImage newPosition = (fst theImage, newPosition)


--6.31 function to change the position of the input inmage to new x,y
-- coordinates passed in as input
moveImage :: Image -> Int -> Int -> Image
moveImage oldImage x y = makeImage (fst oldImage) (x,y)

--6.32 analogue of printPicture TODO: should this print the image in its defined
-- location?
printImage :: Image -> IO ()
printImage theImage = printPicture (fst theImage)
