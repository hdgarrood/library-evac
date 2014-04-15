module Main where

import Keyboard
import Types (..)
import Player (unplayer)
import Floor as F
import GameState as GS
import Random

state : Signal GameState
state = GS.makeState input

maxInt = 2147483647
minInt = -2147483648

input : Signal GameInput
input =
    let tr keyCode dir =
        (\_ -> Move dir) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ SetRandomSeed <~ Random.range minInt maxInt (every (10 * second))
        , TimeStep <~ fps 12
        , tr 38 Up
        , tr 40 Down
        , tr 39 Right
        , tr 37 Left
        ]

renderPlayer : Player -> Form
renderPlayer player = toForm <| image F.tileSize F.tileSize "img/player.png"

renderWorld state =
    let
      p = unplayer state.player
      map =
        collage (.x F.size) (.y F.size)
          [ state |> GS.currentFloor |> F.renderFloor |> toForm
          , renderPlayer state.player |> move (F.getCollageCoords p)
          , state.randomState |> asText |> toForm |> move (20, 20)
          ]
      transition = GS.getTransitionOverlay state
    in
      flow outward [map, transition]

main = renderWorld <~ state
