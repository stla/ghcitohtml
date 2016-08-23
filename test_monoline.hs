

-- define a new type: 
data Point = Point Float Float deriving (Show)
-- variable of type Point:
let point = (Point 3 4)
point
:t point
-- 'Point' is like a function:
:t Point
-- a function acting on 'Point' variables:
:{
let squareNorm :: Point -> Float
    squareNorm (Point x y) = x^2+y^2
:}
squareNorm point
