module GameState where

import Types
import Floor

initialState :: GameState
initialState = GameState
    { statePlayer     = Player (0,0)
    , stateFloors     = initialFloors
    , stateTransition = Nothing
    , stateStdGen     = mkStdGen 0
    }

step :: GameInput -> GameState -> GameState
step input state =
    if inTransition state
        then case input of
            StepTransition -> stepTransition state
            _ -> state
    else case input of
        Move dir ->
            let r = getReader state
            runGameM $ do 
            
            state |> doRandom stepObjects
                  |> preMoveHooks input
                  |> movePlayer dir
                  |> postMoveHooks input
        _ -> state

inTransition :: GameState -> Bool
inTransition = isJust . stateTransition


