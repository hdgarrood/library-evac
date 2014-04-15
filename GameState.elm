module GameState where

import Player (Player, unplayer)
import Geometry as G
import Floor as F
import Types (..)
import ListExtra (safeHead)
import Dict
import DictExtra as Dict
import Zipper
import Pseudorandom
import Pseudorandom (Random, randomInts, randomFloats, pure, (>>=))

initialState : GameState
initialState = { player = Player { x=0, y=0 }
               , floors = F.initialFloors
               , transition = Nothing
               , randomState = 0
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
        SetRandomSeed seed ->
          { state | randomState <- seed }
        Move dir ->
          state |> doRandom stepObjects
                |> preMoveHooks input
                |> movePlayer dir
                |> postMoveHooks input
        _ -> state

-- Perform a computation requiring randomness on the game state.
doRandom : (GameState -> Random GameState) -> GameState -> GameState
doRandom f state = case f state state.randomState of
    (gs, s) -> { gs | randomState <- s }

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
          Upwards   -> Zipper.advance
          Downwards -> Zipper.retreat
      in
        case f state.floors of
          Just floors' -> { state | floors <- floors' }
          Nothing      -> state

currentFloor : GameState -> Floor
currentFloor state = Zipper.current state.floors

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

stepObjects : GameState -> Random GameState
stepObjects state =
    let
      -- possible performance opportunity here
      next = getUnsteppedObject state
    in
      case next of
        Just (floorId, objId) -> stepObject state floorId objId >>= stepObjects
        Nothing               -> pure state

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
    Zipper.values state.floors |> map .floorId

getFloorById : GameState -> FloorId -> Maybe Floor
getFloorById state floorId =
    Zipper.values state.floors |> filter (\f -> f.floorId == floorId)
                               |> safeHead

getObjectById : GameState -> ObjectId -> Maybe Object
getObjectById state objId =
    let l x = case x of
                Just y  -> [y]
                Nothing -> []
    in
      Zipper.values state.floors
        |> concatMap (\f -> f.objects |> Dict.lookup objId |> l)
        |> safeHead

getUnsteppedObjects : GameState -> FloorId -> [(FloorId, ObjectId)]
getUnsteppedObjects state floorId =
    case getFloorById state floorId of
      Just {tiles, objects, floorId} ->
        objects |> Dict.filter (not . .stepped)
                |> Dict.keys
                |> map (\objId -> (floorId, objId))
      Nothing -> []

stepObject : GameState -> FloorId -> ObjectId -> Random GameState
stepObject state floorId objId =
    case getObjectById state objId of
      Just obj -> case obj.typ of
        Fire _ -> stepFireObject state floorId obj
        Person -> stepPersonObject state floorId obj
      Nothing -> pure state

fmap : (a -> b) -> Random a -> Random b
fmap f rand = (\(r, s) -> (f r, s)) . rand

randomInt : Random Int
randomInt = fmap head <| randomInts 1

randomFloat : Random Float
randomFloat = fmap head <| randomFloats 1

stepFireObject : GameState -> FloorId -> Object -> Random GameState
stepFireObject state floorId obj =
  let
    f _ = state -- TODO
  in
    fmap f randomInt

-- TODO
stepPersonObject = stepFireObject

makeState : Signal GameInput -> Signal GameState
makeState input = foldp step initialState input
