module Types where

data Zipper a = Z [a] a [a]

type Positioned a = { a | x:Int, y:Int }
data Player = Player (Positioned {})

data Dir = Left | Right | Up | Down
data FloorTile = Normal | Inaccessible | Stairs StairsDir
data StairsDir = Upwards | Downwards
type Object = Positioned {}
type Floor = { tiles : [[FloorTile]], objects : [Object] }

type GameState = {floors : Zipper Floor, player : Player}
type GameInput = {dir : Dir}
