module Graph
    ( runGraph
    ) where

import Graphics.Gloss ( Display(..), Picture(..)
                      , animate, white
                      )
import Data.Vect (mkVec2, Vec2 (..))
import qualified Tesseract as T

animationSpeed :: Float
animationSpeed = 0.1

display :: Display
display = InWindow "FourD" (400, 400) (100, 100)

runGraph :: IO ()
runGraph = animate display white animation

animation :: Float -> Picture
animation a = picturePolygon2 $ T.square 80 (mkVec2 (0,0)) rot
              where rot = (animationSpeed*a)

picturePolygon2 :: T.Polygon Vec2 -> Picture
picturePolygon2 (T.Polygon vs) = Line $ map unbox (vs ++ [head vs])
                                where unbox (Vec2 x y) = (x,y)


