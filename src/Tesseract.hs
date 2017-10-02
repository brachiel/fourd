{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}

module Tesseract where

import Data.Vect
import Data.Vect.Float.Util.Dim4

data Polygon a = Polygon [a] deriving (Show, Functor)

data Rot3 = Rot3 Vec3 Float

class Embeddable3 a where
    embed :: Float -> a -> Polygon Vec3

instance Embeddable3 (Polygon Vec2) where
    embed z = fmap embedV
            where embedV (Vec2 x y) = Vec3 x y z

-- https://en.wikipedia.org/wiki/Rotations_in_4-dimensional_Euclidean_space

square :: Float -> Vec2 -> Float -> Polygon Vec2
square length center rotation = Polygon $ map ((center &+) . (length/2 *&) . rotate2 rotation) $ structVec2 [-1,-1, -1,1, 1,1, 1,-1]

cube :: Float -> Vec3 -> Rot3 -> [Polygon Vec3]
cube length center (Rot3 axis rotation) = sides
                                          where sides = rotators <*> [side]
                                                rotators = ([rotate] <*> rotations) :: [Polygon Vec3 -> Polygon Vec3]
                                                rotate (a,b) = fmap $ rotate3 b (Vec3 0 1 0) . rotate3 a (Vec3 1 0 0) :: (Float, Float) -> Polygon Vec3 -> Polygon Vec3
                                                rotations = [(0,0),(-1,0),(1,0),(2,0),(0,-1),(0,1)] :: [(Float,Float)]
                                                side = embed 1 $ square length (Vec2 0 0) 0 :: Polygon Vec3
