{-# LANGUAGE FlexibleInstances, DeriveFunctor, MultiParamTypeClasses, FlexibleContexts #-}

module Tesseract where

import Data.Vect hiding (project)
import qualified Data.Vect as V
import Data.Vect.Float.Util.Dim4

data Polygon a = Polygon [a] deriving (Show, Functor)
data Body a = Body [Polygon a] deriving (Show, Functor)

data Rot3 = Rot3 Vec3 Float
noRot3 = Rot3 (Vec3 0 0 0) 0

data Rot4 = Rot4 (Float, Float, Float)

instance (Extend a b, Functor f) => Extend (f a) (f b) where
    extendZero = extendWith 0
    extendWith z = fmap (extendWith z)
    trim = fmap trim


class Flattable f a where
    flatten :: f a -> f Vec2

instance Functor f => Flattable f Vec3 where
    flatten = trim

instance Flattable Polygon Vec4 where
    flatten = trim . (trim :: Polygon Vec4 -> Polygon Vec3)

instance Flattable Body Vec4 where
    flatten = trim . (trim :: Body Vec4 -> Body Vec3)

-- https://en.wikipedia.org/wiki/Rotations_in_4-dimensional_Euclidean_space

-- |A unit square centered at (0 0)
square :: Polygon Vec2
square = Polygon $ structVec2 $ map (/2) [-1,-1, -1,1, 1,1, 1,-1]

-- |Rotate a 3d polygon by the angles a and b around the x and the y axis.
-- where for the angles a and b holds: 1 = 90°, 2 = 180°
rotate3xy :: (Float, Float) -> Vec3 -> Vec3
rotate3xy (a,b) = rotate3 (pi/2*b) vec3Y
                . rotate3 (pi/2*a) vec3X


translate :: Vector v => v -> v -> v
translate = (&+)

stretch :: Vector v => Float -> v -> v
stretch = (*&)

rotate4xyz :: (Float, Float, Float) -> Vec4 -> Vec4
rotate4xyz (a,b,c) = rotate4 (pi/2*c) (vec4Z, vec4W)
                   . rotate4 (pi/2*b) (vec4Y, vec4W)
                   . rotate4 (pi/2*a) (vec4X, vec4W)

-- |A unit cube, centered at (0 0 0)
-- polygons
cube :: Body Vec3
cube = Body $ rotators <*> [side]
       where rotators = map fmap $ [rotate3xy] <*> rotations
             rotations = [(0,0),(-1,0),(1,0),(2,0),(0,-1),(0,1)]
             -- Creates a side which is then replicated and rotated around
             side = extendWith (1/2) square

transformedCube :: Float -> Vec3 -> Rot3 -> Body Vec3
transformedCube x v (Rot3 a r) = fmap (rotate3 r a . translate v . stretch x) cube

-- |A unit tesseract, centered at (0 0 0 0)
tesseract :: Body Vec4
tesseract = Body $ rotators <*> sidePolys
            where rotators = (fmap fmap ([rotate4xyz] <*> rotations)) :: [Polygon Vec4 -> Polygon Vec4]
                  rotations = [(0,0,0)
                              ,(0,0,1) ,(0,1,0) ,(1,0,0)
                              ,(0,0,-1),(0,-1,0),(-1,0,0)
                              ,(0,0,2)]
                  -- Creates a side which is then replicated and rotated around
                  Body sidePolys = extendWith (1/2) $ cube


transformedTesseract :: Float -> Vec4 -> Rot4 -> Body Vec4
transformedTesseract x v (Rot4 r) = fmap (rotate4xyz r . translate v . stretch x) tesseract

