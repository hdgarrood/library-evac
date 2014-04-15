module Main where

import Keyboard
import Types (..)
import Player (unplayer)
import Extra as E
import Floor as F
import GameState as GS

state : Signal GameState
state = GS.makeState input

input : Signal GameInput
input =
    let tr keyCode dir =
        (\_ -> Move dir) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ TimeStep <~ fps 12
        , tr 38 Up
        , tr 40 Down
        , tr 39 Right
        , tr 37 Left
        ]

renderPlayer : Player -> Form
renderPlayer player = filled red (square F.tileSize)

renderWorld state =
    let
      p = unplayer state.player
      map =
        collage (.x F.size) (.y F.size)
          [ state |> GS.currentFloor |> F.renderFloor |> toForm
          , renderPlayer state.player |> move (F.getCollageCoords p)
          , toForm <| asText <| .label <| GS.currentFloor <| state
          ]
      transition = GS.getTransitionOverlay state
    in
      flow outward [map, transition]

main = renderWorld <~ state
