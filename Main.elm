module Main where

import Keyboard
import Geometry as G
import Floor as F

colours = { tan=rgb 255 190 120, blue=rgb 12 100 200, red=red}

data Player = Player (G.Positioned {})

unplayer : Player -> G.Positioned {}
unplayer (Player r) = r

initialState : GameState
initialState = {player = Player { x=0, y=0 }, floor = F.initialFloor}

renderPlayer : Player -> Form
renderPlayer player = filled colours.red (square F.tileSize)

type GameState = {floor:F.Floor, player:Player}
type GameInput = {dir:G.Dir}

step : GameInput -> GameState -> GameState
step {dir} {floor, player} =
    if canMove floor player dir
      then case player of
        Player rec -> {floor = floor, player = Player <| G.moveDir dir rec}
      else {floor = floor, player = player}


canMove : F.Floor -> Player -> G.Dir -> Bool
canMove floor (Player pos) dir =
    let
      newpos = G.moveDir dir pos
    in
      F.occupiableAt floor newpos

input : Signal GameInput
input =
    let f dir keyCode =
        (\_ -> {dir=dir}) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ f G.Up 38
        , f G.Down 40
        , f G.Right 39
        , f G.Left 37
        ]

state : Signal GameState
state = foldp step initialState input

renderWorld state =
    let
      p = unplayer state.player
    in
      collage (.x F.size) (.y F.size)
        [ toForm <| F.renderFloor state.floor
        , renderPlayer state.player |> move (F.getCollageCoords p)
        , toForm <| asText state.player
        ]

main = renderWorld <~ state
