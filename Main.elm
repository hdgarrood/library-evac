module Main where

import Keyboard
import Types (..)
import Player (unplayer)
import Extra as E
import Floor as F
import GameState (state)

input : Signal GameInput
input =
    let f dir keyCode =
        (\_ -> {dir=dir}) <~ keepIf id True (Keyboard.isDown keyCode)
    in merges
        [ f Up 38
        , f Down 40
        , f Right 39
        , f Left 37
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
        , toForm <| asText state.player
        ]

main = renderWorld <~ state input
