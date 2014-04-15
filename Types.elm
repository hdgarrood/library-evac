module Types where

data Zipper a = Z [a] a [a]

type Positioned a = { a | x:Int, y:Int }
data Player = Player (Positioned {})

data Dir = Left | Right | Up | Down
data FloorTile = Normal | Inaccessible | Stairs StairsDir
data StairsDir = Upwards | Downwards
type Object = Positioned {}
type Floor = { tiles : [[FloorTile]], objects : [Object], label : String }

type Transition = { reason : TransitionReason
                  , performed : Bool
                  , remaining : Time
                  }
data TransitionReason = UsingStairs StairsDir

type GameState = { floors : Zipper Floor
                 , player : Player
                 , transition : Maybe Transition
                 }

data GameInput = Move Dir | TimeStep Time
