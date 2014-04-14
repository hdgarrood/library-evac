module Floor where

import Extra ((!!))
import Geometry as G

data FloorTile = Floor | Space
type Floor = [[FloorTile]]

tileSize = 32

-- Should both be odd (for now)
dimensions : { x:Int, y:Int }
dimensions = { x=29, y=19 }

size = { x = dimensions.x * tileSize, y = dimensions.y * tileSize }

initialFloor : Floor
initialFloor =
    let
      x = 7
      width = 4
      y = 9
      height = 4
      fullCol = repeat dimensions.y Floor
      colWithSpace = repeat y Floor ++
                        repeat height Space ++
                        repeat (dimensions.y - height - y) Floor

      cols1 = repeat x fullCol
      cols2 = repeat width colWithSpace
      cols3 = repeat (dimensions.x - x - width) fullCol
    in
      cols1 ++ cols2 ++ cols3

occupiable : FloorTile -> Bool
occupiable t = case t of
    Floor -> True
    Space -> False

occupiableAt : Floor -> G.Positioned a -> Bool
occupiableAt floor pos =
    case tileAt floor pos of
      Just t -> occupiable t
      Nothing -> False

tileAt : Floor -> G.Positioned a -> Maybe FloorTile
tileAt floor {x, y} =
    case floor !! x of
      Just xs -> xs !! y
      Nothing -> Nothing

-- RENDERING
renderFloor : Floor -> Element
renderFloor f =
  let cols = f
  in  flow right (map renderCol cols)

renderCol : [FloorTile] -> Element
renderCol col = flow down (map toElement col)

toElement : FloorTile -> Element
toElement tile =
    let
      sh = square tileSize
      f fill outline =
        collage tileSize tileSize
        [ sh |> filled fill
        , sh |> outlined (solid outline)
        ]
    in
      case tile of
        Floor -> f orange blue
        Space -> f black white
