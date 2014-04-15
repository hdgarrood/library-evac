module GameState where

import Extra as E
import Player (Player, unplayer)
import Geometry as G
import Floor as F
import Types (..)

initialState : GameState
initialState = { player = Player { x=0, y=0 }
               , floors = F.initialFloors
               , transition = Nothing
               }

inTransition : GameState -> Bool
inTransition state = isJust state.transition

getTransitionOverlay : GameState -> Element
getTransitionOverlay state =
    let
      x = F.size |> .x
      y = F.size |> .y
      overlay t = spacer x y |> color black |> opacity (getOpacity t)
    in
      case state.transition of
        Just t  -> overlay t
        Nothing -> empty

getOpacity : Transition -> Float
getOpacity t = 1 - abs (t.remaining - 0.5)

step : GameInput -> GameState -> GameState
step input state =
    if inTransition state
      then case input of
        TimeStep delta -> stepTransition delta state
        _ -> state
      else case input of
        Move dir -> state |> updateObjects
                          |> preMoveHooks input
                          |> movePlayer dir
                          |> postMoveHooks input
        _ -> state

transitionLength = 1.0

stepTransition : Time -> GameState -> GameState
stepTransition delta state =
    let
      newTime rem = rem - (inSeconds delta / transitionLength)

      newTransition transition rem' =
        if rem' <= 0
          then Nothing
          else Just { transition | remaining <- rem' }

      newState transition =
        let
          rem' = newTime transition.remaining
          state' = { state | transition <- newTransition transition rem' }
        in
          if rem' <= 0.5 && not transition.performed
            then state' |> updateForReason transition.reason |> markPerformed
            else state'

      markPerformed state =
        case state.transition of
          Just t -> { state | transition <- Just { t | performed <- True } }
          Nothing -> state
    in
      case state.transition of
        Just t -> newState t
        Nothing -> state

updateForReason : TransitionReason -> GameState -> GameState
updateForReason reason state = case reason of
    UsingStairs dir ->
      let
        f = case dir of
          Upwards   -> E.advance
          Downwards -> E.retreat
      in
        case f state.floors of
          Just floors' -> { state | floors <- floors' }
          Nothing      -> state

currentFloor : GameState -> Floor
currentFloor state = E.current state.floors

movePlayer : Dir -> GameState -> GameState
movePlayer dir state =
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

stepObjects : GameState -> GameState
stepObjects state =
    let
      -- possible performance opportunity here
      next = getUnsteppedObject state
    in
      case next of
        Just (floorId, objId) -> stepObjects (stepObject state floorId objId)
        Nothing               -> state

getUnsteppedObject : GameState -> Maybe (FloorId, ObjectId)
getUnsteppedObject state =
    let
      floorIds = getFloorIds state
    in
      case concatMap (getUnsteppedObjects state) floorIds of
        x::_ -> Just x
        []   -> Nothing

getFloorIds : GameState -> [FloorId]
getFloorIds state =
    E.values state.floors |> map .floorId

getUnsteppedObjects : GameState -> FloorId -> [(FloorId, ObjectId)]
getUnsteppedObjects state floorId =

stepObject : GameState -> FloorId -> ObjectId -> GameState
stepObject state floorId objId = state

makeState : Signal GameInput -> Signal GameState
makeState input = foldp step initialState input
