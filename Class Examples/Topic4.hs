
-- Take cross product of two 3-tuples
cross :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
cross (x1, y1, z1) (x2, y2, z2) = (y1*z2-z1*y2, z1*x2-x1*z2, x1*y2-y1*x2)
-- could also do: cross a b   but this isn't useful here

dot :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

-- Compute length of a vector
len :: (Double, Double, Double) - > Double
len (x,y,z) = sqrt(x*x + y*y + z*z)
