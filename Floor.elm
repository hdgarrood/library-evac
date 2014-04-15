module Floor where

import Types (..)
import Extra ((!!), Z, advance, retreat)
import Geometry as G
import Dict as D

-- Should both be odd (for now)
dimensions : { x:Int, y:Int }
dimensions = { x=29, y=19 }

-- In pixels; for rendering
tileSize = 32
size = { x = dimensions.x * tileSize, y = dimensions.y * tileSize }

groundFloor : Floor
groundFloor =
    let
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
    in
      { label = "G", tiles = cols1 ++ cols2 ++ cols3, objects = D.empty }

firstFloor : Floor
firstFloor =
    let
      x = 23
      width = 4
      y = 12
      height = 4
      fullCol = repeat dimensions.y Normal
      colWithStairs = repeat 3 Normal ++
                        [Stairs Downwards] ++
                        repeat (dimensions.y - 4) Normal
      colWithObstacle = repeat y Normal ++
                        repeat height Inaccessible ++
                        repeat (dimensions.y - height - y) Normal

      cols1 = fullCol :: colWithStairs :: repeat (x-2) fullCol
      cols2 = repeat width colWithObstacle
      cols3 = repeat (dimensions.x - x - width) fullCol
    in
      { label = "1", tiles = cols1 ++ cols2 ++ cols3, objects = D.empty }

initialFloors = Z [] groundFloor [firstFloor]

occupiable : FloorTile -> Bool
occupiable t = case t of
    Normal       -> True
    Inaccessible -> False
    Stairs _     -> True

occupiableAt : Floor -> Positioned a -> Bool
occupiableAt floor pos =
    case tileAt floor pos of
      Just t -> occupiable t
      Nothing -> False

tileAt : Floor -> Positioned a -> Maybe FloorTile
tileAt {tiles, objects} {x, y} =
    case tiles !! x of
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
getCollageCoords : Positioned a -> (Float, Float)
getCollageCoords {x,y} =
   let translate length = (\v -> v - (length - 1) `div` 2)
       adjust length v = v |> translate length
                           |> (\v -> v * tileSize)
                           |> toFloat
   in (adjust dimensions.x x, adjust dimensions.y y)

toForms : (Positioned a, FloorTile) -> [Form]
toForms (pos, tile) =
    let
      forms path = [ toForm <| image tileSize tileSize path ]
      f path = forms path |> map (move <| getCollageCoords pos)
    in
      f <| imgPathFor tile

imgPathFor : FloorTile -> String
imgPathFor tile =
  case tile of
    Normal           -> "img/carpet.png"
    Inaccessible     -> "img/books.png"
    Stairs Upwards   -> "img/stairs-up.png"
    Stairs Downwards -> "img/stairs-down.png"

onLeave : FloorTile -> GameState -> GameState
onLeave _ state = state

onEnter : FloorTile -> GameState -> GameState
onEnter tile state =
    case tile of
      Stairs dir -> takeStairs dir state
      _          -> state

takeStairs : StairsDir -> GameState -> GameState
takeStairs dir state =
    let
      newTransition =
        Just { remaining = 1.0, reason = UsingStairs dir, performed = False }
    in
      { state | transition <- newTransition }
