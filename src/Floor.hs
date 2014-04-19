{-# LANGUAGE OverloadedStrings #-}

module Floor where

import Control.Arrow
import qualified Data.Map as Map

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
groundFloor = makeEmptyFloor $ makeCols 7 9 4 4 3 Upwards

firstFloor :: Floor
firstFloor = makeEmptyFloor $ makeCols 2 7 4 4 3 Downwards

initialFloors :: FloorCollection
initialFloors = FloorCollection
    { collectionFloors = Map.fromList [("G", groundFloor), ("1", firstFloor)]
    , collectionZipper = Z [] "G" ["1"]
    }

makeEmptyFloor :: [[FloorTile]] -> Floor
makeEmptyFloor tiles = Floor
    { floorTiles = tiles
    , floorObjects = Map.empty
    , floorLastObjectId = ObjectId 0
    }

makeCols :: Int -> Int -> Int -> Int -> Int -> StairsDir -> [[FloorTile]]
makeCols x y width height stairsY stairsDir = cols1 ++ cols2 ++ cols3
    where
    fullCol = replicate (getY dimensions) Normal
    colWithStairs = replicate stairsY Normal ++
                      [Stairs stairsDir] ++
                      replicate (getY dimensions - 4) Normal
    colWithObstacle = replicate y Normal ++
                      replicate height Inaccessible ++
                      replicate (getY dimensions - height - y) Normal

    cols1 = fullCol : colWithStairs : replicate (x-2) fullCol
    cols2 = replicate width colWithObstacle
    cols3 = replicate (getX dimensions - x - width) fullCol
