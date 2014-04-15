module GameState where

import Extra as E
import Player (Player, unplayer)
import Geometry as G
import Floor as F
import Types (..)

initialState : GameState
initialState = {player = Player { x=0, y=0 }, floors = F.initialFloors}

step : GameInput -> GameState -> GameState
step input state = state |> preMoveHooks input
                         |> movePlayer input
                         |> postMoveHooks input

currentFloor : GameState -> Floor
currentFloor state = E.current state.floors

movePlayer : GameInput -> GameState -> GameState
movePlayer {dir} state =
    let
      floor  = currentFloor state
      player = state.player
      pos    = unplayer player
      newpos = G.moveDir dir pos
    in
      if canMove floor player dir
        then { state | player <- Player newpos }
        else state

getTileUnderPlayer : GameState -> FloorTile
getTileUnderPlayer state =
    let
      pos = unplayer state.player
    in
      case F.tileAt (currentFloor state) pos of
        Just t  -> t
        Nothing -> Normal -- HACK

moveHook : (FloorTile -> GameState -> GameState) -> GameState -> GameState
moveHook f state =
    let
      tile = getTileUnderPlayer state
    in
      f tile state

preMoveHooks : GameInput -> GameState -> GameState
preMoveHooks _ = moveHook F.onLeave

postMoveHooks : GameInput -> GameState -> GameState
postMoveHooks _ = moveHook F.onEnter

canMove : Floor -> Player -> Dir -> Bool
canMove floor player dir =
    let
      pos = unplayer player
      newpos = G.moveDir dir pos
    in
      F.occupiableAt floor newpos

state : Signal GameInput -> Signal GameState
state input = foldp step initialState input
