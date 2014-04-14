import Keyboard

colours = { tan=rgb 255 190 120, blue=rgb 12 100 200, red=red}

data Dir = Left | Right | Up | Down
data FloorTile = Floor
tileSize = 32

type Positioned a = { a | x:Int, y:Int }

toPositioned : Dir -> Positioned {}
toPositioned d = case d of
    Left  -> { x=-1, y=0  }
    Right -> { x=1,  y=0  }
    Up    -> { x=0,  y=1  }
    Down  -> { x=0,  y=-1 }

fromPositioned : Positioned {} -> Maybe Dir
fromPositioned {x,y} = if | x /= 0    -> Just <| if x > 0 then Right else Left
                          | y /= 0    -> Just <| if y > 0 then Up    else Down
                          | otherwise -> Nothing

vecAdd : Positioned a -> Positioned b -> Positioned b
a `vecAdd` b = { b | x <- a.x + b.x,
                     y <- a.y + b.y }

-- Should both be odd (for now)
floorDimensions : { x:Int, y:Int }
floorDimensions = { x=29, y=19 }

floorSize = { x=floorDimensions.x * tileSize, y=floorDimensions.y * tileSize }

type Floor = [[FloorTile]]

floor : Floor
floor = repeat floorDimensions.x (repeat floorDimensions.y Floor)

data Player = Player (Positioned {})

initialPlayer : Player
initialPlayer = Player { x=0, y=0 }

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
  in
    collage tileSize tileSize
    [ sh |> filled colours.tan
    , sh |> outlined (solid colours.blue)
    ]

renderPlayer : Player -> Form
renderPlayer player = filled colours.red (square tileSize)

getCollageCoords : Player -> (Float, Float)
getCollageCoords (Player {x,y}) =
   let translate length = (\v -> v - (length - 1) `div` 2)
       adjust length v = v |> translate length
                           |> (\v -> v * tileSize)
                           |> toFloat
   in (adjust floorDimensions.x x, adjust floorDimensions.y y)

moveDir : Dir -> Positioned a -> Positioned a
moveDir dir pos = toPositioned dir `vecAdd` pos

step : Dir -> Player -> Player
step input (Player rec) = Player <| moveDir input rec

input : Signal Dir
input =
    let f dir keyCode =
        (\_ -> dir) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ f Up 38
        , f Down 40
        , f Right 39
        , f Left 37
        ]

sPlayer : Signal Player
sPlayer = foldp step initialPlayer input

renderWorld player = collage floorSize.x floorSize.y
  [ toForm <| renderFloor floor
  , renderPlayer player |> move (getCollageCoords player)
  , toForm <| asText player
  ]

main = renderWorld <~ sPlayer
