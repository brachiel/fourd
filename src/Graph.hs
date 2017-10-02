module Graph
    ( runGraph
    ) where

import Graphics.Gloss ( Display(..), Picture(..)
                      , animate, white
                      )
import Data.Vect ( mkVec2, Vec2 (..)
                 , mkVec3, Vec3 (..)
                 )
import qualified Tesseract as T

animationSpeed :: Float
animationSpeed = 0.3

display :: Display
display = InWindow "FourD" (400, 400) (100, 100)

runGraph :: IO ()
runGraph = animate display white animation

animation :: Float -> Picture
animation s = polygon3ToPicture $ animationCube (animationSpeed*s)
--animation s = polygon2ToPicture $ animationSquare (animationSpeed*s)


animationSquare :: Float -> T.Polygon Vec2
animationSquare = T.square 80 (mkVec2 (0,0))


animationCube :: Float -> [T.Polygon Vec3]
animationCube r = T.cube 80 (mkVec3 (0,0,0)) (T.Rot3 (mkVec3 (0.2,0.2,0.2)) r)


polygon2ToPicture :: T.Polygon Vec2 -> Picture
polygon2ToPicture (T.Polygon vs) = Line $ map unbox (vs ++ [head vs])
                                   where unbox (Vec2 x y) = (x,y)

polygon3ToPicture :: [T.Polygon Vec3] -> Picture
polygon3ToPicture vs = Pictures $ map (polygon2ToPicture . T.proj) vs
