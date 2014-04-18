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

movePlayer : Dir -> GameState -> GameState
movePlayer dir state =
    let
      floor  = F.currentFloor state
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
      case F.tileAt (F.currentFloor state) pos of
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
      floorIds = F.getFloorIds state.floors
    in
      case concatMap (getUnsteppedObjects state) floorIds of
        x::_ -> Just x
        []   -> Nothing

modifyFloor : GameState -> FloorId -> (Floor -> Floor) -> GameState
modifyFloor state floorId f =
    case F.getFloor floorId state.floors of
      Just floor ->
        { state | floors <- F.modifyFloor floorId f state.floors }
      Nothing -> state

getObjectById : GameState -> ObjectId -> Maybe Object
getObjectById state objId =
    let l x = case x of
                Just y  -> [y]
                Nothing -> []
    in
      Dict.values state.floors.dict
        |> concatMap (\f -> f.objects |> Dict.lookup objId |> l)
        |> safeHead

getUnsteppedObjects : GameState -> FloorId -> [(FloorId, ObjectId)]
getUnsteppedObjects state floorId =
    case F.getFloor floorId state.floors of
      Just floor ->
        floor.objects |> Dict.filter (not . .stepped)
                      |> Dict.keys
                      |> map (\objId -> (floorId, objId))
      Nothing -> []

stepObject : GameState -> FloorId -> ObjectId -> Random GameState
stepObject state floorId objId =
    case getObjectById state objId of
      Just obj -> case obj.typ of
        Fire _ -> stepFireObject state floorId objId
        Person -> stepPersonObject state floorId objId
      Nothing -> pure state

fmap : (a -> b) -> Random a -> Random b
fmap f rand = (\(r, s) -> (f r, s)) . rand

randomInt : Random Int
randomInt = fmap head <| randomInts 1

randomFloat : Random Float
randomFloat = fmap head <| randomFloats 1

randomRange : (Int, Int) -> Random Int
randomRange range = fmap head <| Pseudorandom.randomRange range 1

stepFireObject : GameState -> FloorId -> ObjectId -> Random GameState
stepFireObject state floorId objId =
  let
    toDir x = case x of
                 0 -> Up
                 1 -> Left
                 2 -> Down
                 3 -> Right
                 _ -> Up
    -- TODO: Intensity should affect likelihood of action
    action r = if | r < 20 -> Just <| Spread <| toDir <| r `div` 5
                  | r < 30 -> Just Grow
                  | otherwise -> Nothing
    f r = case action r of
        Just a -> applyFireAction state floorId objId a
        Nothing -> state
  in
    fmap f <| randomRange (0,99)

grow : FireIntensity -> FireIntensity
grow i = case i of
    I1 -> I2
    I2 -> I3
    I3 -> I4
    I4 -> I5
    I5 -> I5

applyFireAction : GameState -> FloorId -> ObjectId -> FireAction -> GameState
applyFireAction state floorId objId action = state

-- TODO
stepPersonObject = stepFireObject

makeState : Signal GameInput -> Signal GameState
makeState input = foldp step initialState input
