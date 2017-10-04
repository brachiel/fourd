{-# LANGUAGE FlexibleInstances, DeriveFunctor, MultiParamTypeClasses, FlexibleContexts #-}

module Tesseract where

import Data.Vect hiding (project)
import qualified Data.Vect as V
import Data.Vect.Float.Util.Dim4

data Polygon a = Polygon [a] deriving (Show, Functor)
data Body a = Body [Polygon a] deriving (Show, Functor)

data Rot3 = Rot3 Vec3 Float
noRot3 = Rot3 (Vec3 0 0 0) 0

instance (Extend a b, Functor f) => Extend (f a) (f b) where
    extendZero = extendWith 0
    extendWith z = fmap (extendWith z)
    trim = fmap trim


class Flattable f a where
    flatten :: f a -> f Vec2

instance Functor f => Flattable f Vec3 where
    flatten = trim

instance Flattable Body Vec4 where
    flatten = trim . (trim :: Body Vec4 -> Body Vec3)

-- https://en.wikipedia.org/wiki/Rotations_in_4-dimensional_Euclidean_space

square :: Float -> Vec2 -> Float -> Polygon Vec2
square length center rotation 
    = Polygon $ map ((center &+) . (length/2 *&) . rotate2 rotation) unitSquare
      where unitSquare = structVec2 [-1,-1, -1,1, 1,1, 1,-1]

-- |Rotate a 3d polygon by the angles a and b around the x and the y axis.
-- where for the angles a and b holds: 1 = 90°, 2 = 180°
rotate3xy :: (Float, Float) -> Vec3 -> Vec3
rotate3xy (a,b) = rotate3 (pi/2*b) (Vec3 0 1 0) 
                . rotate3 (pi/2*a) (Vec3 1 0 0)

translate3 :: Vec3 -> Vec3 -> Vec3
translate3 = (&+)

stretch3 :: Float -> Vec3 -> Vec3
stretch3 = (*&)

-- |A cube of length, position and rotation represented by a list of Vec3 
-- polygons
cube :: Body Vec3
cube = Body $ rotators <*> [side]
       where rotators = map fmap $ [rotate3xy] <*> rotations
             rotations = [(0,0),(-1,0),(1,0),(2,0),(0,-1),(0,1)]
             -- Creates a side which is then replicated and rotated around
             side = extendWith (1/2) $ square 1 (Vec2 0 0) 0 

transformedCube :: Float -> Vec3 -> Rot3 -> Body Vec3
transformedCube x v (Rot3 a r) = fmap (rotate3 r a . translate3 v . stretch3 x) $ cube

