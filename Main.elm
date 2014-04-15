module Main where

import Keyboard
import Types (..)
import Player (unplayer)
import Extra as E
import Floor as F
import GameState (state)

input : Signal GameInput
input =
    let tr keyCode dir =
        (\_ -> {dir=dir}) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ tr 38 Up
        , tr 40 Down
        , tr 39 Right
        , tr 37 Left
        ]

renderPlayer : Player -> Form
renderPlayer player = filled red (square F.tileSize)

renderWorld state =
    let
      p = unplayer state.player
    in
      collage (.x F.size) (.y F.size)
        [ state.floors |> E.current |> F.renderFloor |> toForm
        , renderPlayer state.player |> move (F.getCollageCoords p)
        ]

main = renderWorld <~ state input
