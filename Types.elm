module Types where

import Dict (Dict)
import Zipper (Zipper)

type Positioned a = { a | x:Int, y:Int }
data Player = Player (Positioned {})

data Dir = Left | Right | Up | Down
data FloorTile = Normal | Inaccessible | Stairs StairsDir
data StairsDir = Upwards | Downwards

type Object = Positioned {typ : ObjectType, stepped : Bool}
data ObjectType = Fire FireIntensity | Person
data FireIntensity = I1 | I2 | I3 | I4 | I5
data FireAction = Grow | Spread Dir

type Floor = { tiles : [[FloorTile]]
             , objects : Dict ObjectId Object
             , lastObjectId : ObjectId
             }

type FloorCollection = { dict : Dict FloorId Floor, zipper : Zipper FloorId }

type ObjectId = Int
type FloorId = String

type Transition = { reason : TransitionReason
                  , performed : Bool
                  , remaining : Time
                  }
data TransitionReason = UsingStairs StairsDir

type GameState = { floors : FloorCollection
                 , player : Player
                 , transition : Maybe Transition
                 , randomState : Int
                 }

data GameInput = Move Dir | TimeStep Time | SetRandomSeed Int
