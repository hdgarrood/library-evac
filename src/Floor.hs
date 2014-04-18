module Floor where

import Control.Arrow

import Types
import Zipper

dimensions :: Vec
dimensions = (29, 19)

-- The following are in pixels and are used for rendering
tileSize :: Int
tileSize = 32

canvasSize :: Vec
canvasSize = both (* tileSize) dimensions 
    where both f = f *** f

groundFloor :: Floor
groundFloor = Floor { floorTiles = cols1 ++ cols2 ++ cols3
                    , floorObjects = Dict.empty
                    , floorLastObjectId = 0
                    }
    where
    x = 7
    width = 4
    y = 9
    height = 4
    fullCol = repeat dimensions.y Normal
    colWithStairs = repeat 3 Normal ++
                      [Stairs Upwards] ++
                      repeat (dimensions.y - 4) Normal
    colWithObstacle = repeat y Normal ++
                      repeat height Inaccessible ++
                      repeat (dimensions.y - height - y) Normal

    cols1 = fullCol :: colWithStairs :: repeat (x-2) fullCol
    cols2 = repeat width colWithObstacle
    cols3 = repeat (dimensions.x - x - width) fullCol
    
