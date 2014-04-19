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
