module Floor where

import Extra ((!!))
import Geometry as G

data FloorTile = Floor | Space
type Floor = [[FloorTile]]

-- Should both be odd (for now)
dimensions : { x:Int, y:Int }
dimensions = { x=29, y=19 }

-- In pixels; for rendering
tileSize = 32
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
  let
    pairs = concatMap (\x -> map ((,) x) [0..dimensions.y]) [0..dimensions.x]
    tiles = pairs
            |> map (\(x, y) -> {x=x, y=y})
            |> map (\pos -> case tileAt f pos of
                                Just t -> Just (pos, t)
                                Nothing -> Nothing)
            |> justs
    forms = concatMap toForms tiles
  in collage size.x size.y forms

-- This implementation requires the floor dimensions to be odd.
getCollageCoords : G.Positioned a -> (Float, Float)
getCollageCoords {x,y} =
   let translate length = (\v -> v - (length - 1) `div` 2)
       adjust length v = v |> translate length
                           |> (\v -> v * tileSize)
                           |> toFloat
   in (adjust dimensions.x x, adjust dimensions.y y)

toForms : (G.Positioned a, FloorTile) -> [Form]
toForms (pos, tile) =
    let
      forms path = [ toForm <| image tileSize tileSize path ]
      f path = forms path |> map (move <| getCollageCoords pos)
    in
      case tile of
        Floor -> f "/img/carpet.png"
        Space -> f "/img/books.png"
