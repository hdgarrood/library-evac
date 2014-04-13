import Keyboard
import Window

-- COLOURS
colours = { tan=rgb 255 190 120, blue=rgb 12 100 200, red=red}

-- MODEL
data FloorTile = Floor
tileSize = 36

-- Should both be odd (for now)
floorDimensions : { x:Int, y:Int }
floorDimensions = { x=15, y=15 }

floorSize = { x=floorDimensions.x * tileSize, y=floorDimensions.y * tileSize }

type Floor = [[FloorTile]]
floor = repeat floorDimensions.x (repeat floorDimensions.y Floor)

initialPlayer : { x:Int, y:Int }
initialPlayer = { x=0, y=0 }

renderFloor f =
  let cols = f
  in  flow right (map renderCol cols)

renderCol col = flow down (map toElement col)
toElement tile =
  let
    sh = square tileSize
  in
    collage tileSize tileSize
    [ sh |> filled colours.tan
    , sh |> outlined (solid colours.blue)
    ]

renderPlayer player = filled colours.red (square tileSize)

getCollageCoords {x,y} =
   let translate length = (\v -> v - (length - 1) `div` 2)
       adjust length v = v |> translate length |> (\v -> v * tileSize) |> toFloat
   in (adjust floorDimensions.x x, adjust floorDimensions.y y)

step input player = { x=player.x + input.x, y=player.y + input.y }

signalPlayer = foldp step initialPlayer Keyboard.arrows

renderWorld player =collage floorSize.x floorSize.y
  [ toForm <| renderFloor floor
  , renderPlayer player |> move (getCollageCoords player)
  ]

main = renderWorld <~ signalPlayer
