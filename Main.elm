module Main where

import Keyboard
import Extra as E
import Geometry as G
import Floor as F

data Player = Player (G.Positioned {})

unplayer : Player -> G.Positioned {}
unplayer (Player r) = r

type GameState = {floors : E.Zipper F.Floor, player : Player}
type GameInput = {dir : G.Dir}

initialState : GameState
initialState = {player = Player { x=0, y=0 }, floors = F.initialFloors}

renderPlayer : Player -> Form
renderPlayer player = filled red (square F.tileSize)

step : GameInput -> GameState -> GameState
step {dir} state =
    let
      floor = E.current state.floors
      player = state.player
    in
      if canMove floor player dir
        then { state | player <- Player <|
                        G.moveDir dir (unplayer player) }
        else state

canMove : F.Floor -> Player -> G.Dir -> Bool
canMove floor player dir =
    let
      pos = unplayer player
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
        [ state.floors |> E.current |> F.renderFloor |> toForm
        , renderPlayer state.player |> move (F.getCollageCoords p)
        , toForm <| asText state.player
        ]

main = renderWorld <~ state
