{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}

module Tesseract where

import Data.Vect
import Data.Vect.Float.Util.Dim4

data Polygon a = Polygon [a] deriving (Show, Functor)
data Body a = Body [Polygon a] deriving (Show, Functor)

data Rot3 = Rot3 Vec3 Float

class Embeddable3 a where
    embed :: Float -> a -> Polygon Vec3

instance Embeddable3 (Polygon Vec2) where
    embed z = fmap embedV
            where embedV (Vec2 x y) = Vec3 x y z

class Projectable2 a where
    proj :: a -> Polygon Vec2

instance Projectable2 (Polygon Vec3) where
    proj = fmap $ dropZ . flip project (Vec3 0 0 1)
           where dropZ (Vec3 x y _) = Vec2 x y

-- https://en.wikipedia.org/wiki/Rotations_in_4-dimensional_Euclidean_space

square :: Float -> Vec2 -> Float -> Polygon Vec2
square length center rotation 
    = Polygon $ map ((center &+) . (length/2 *&) . rotate2 rotation) unitSquare
      where unitSquare = structVec2 [-1,-1, -1,1, 1,1, 1,-1]

-- |Rotate a 3d polygon by the angles a and b around the x and the y axis.
-- where for the angles a and b holds: 1 = 90°, 2 = 180°
rotate3d :: (Float, Float) -> Polygon Vec3 -> Polygon Vec3
rotate3d (a,b) = fmap $ rotate3 (pi/2*b) (Vec3 0 1 0) 
                      . rotate3 (pi/2*a) (Vec3 1 0 0)

-- |A cube of length, position and rotation represented by a list of Vec3 
-- polygons
cube :: Float -> Vec3 -> Rot3 -> Body Vec3
cube length center (Rot3 axis rotation) 
    = fmap (rotate3 rotation axis) sides
      where sides = Body $ rotators <*> [side] :: Body Vec3
            rotators = [rotate3d] <*> rotations
            rotations = [(0,0),(-1,0),(1,0),(2,0),(0,-1),(0,1)]
            -- Creates a side which is then replicated and rotated around
            side = embed (length/2) $ square length (Vec2 0 0) 0 

