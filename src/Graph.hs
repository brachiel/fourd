module Graph
    ( runGraph
    ) where

import Graphics.Gloss ( Display(..), Picture(..)
                      , animate, white
                      )
import Data.Vect ( mkVec2, Vec2 (..)
                 , mkVec3, Vec3 (..)
                 , mkVec4, Vec4 (..)
                 , Extend
                 )
import qualified Tesseract as T

animationSpeed :: Float
animationSpeed = 0.3

display :: Display
display = InWindow "FourD" (400, 400) (100, 100)

runGraph :: IO ()
runGraph = animate display white animation

animation :: Float -> Picture
animation s = body4ToPicture $ animationTesseract (animationSpeed*s)
--animation s = body3ToPicture $ animationCube (animationSpeed*s)
--animation s = polygon2ToPicture $ animationSquare (animationSpeed*s)


animationCube :: Float -> T.Body Vec3
animationCube r = T.transformedCube 80 (mkVec3 (0,0,0)) (T.Rot3 (mkVec3 (0.2,0.2,0.2)) r)

animationTesseract :: Float -> T.Body Vec4
animationTesseract r = T.transformedTesseract 80 (mkVec4 (0,0,0,0)) (T.Rot4 (0.2,0.2*r,r))

polygon2ToPicture :: T.Polygon Vec2 -> Picture
polygon2ToPicture (T.Polygon vs) = Line $ map unbox (vs ++ [head vs])
                                   where unbox (Vec2 x y) = (x,y)

body3ToPicture :: T.Body Vec3 -> Picture
body3ToPicture (T.Body vs) = Pictures $ map (polygon2ToPicture . T.flatten) vs

body4ToPicture :: T.Body Vec4 -> Picture
body4ToPicture (T.Body vs) = Pictures $ map (polygon2ToPicture . T.flatten) vs

