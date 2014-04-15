module Geometry where

import Types (..)

toPositioned : Dir -> Positioned {}
toPositioned d = case d of
    Left  -> { x=-1, y=0  }
    Right -> { x=1,  y=0  }
    Up    -> { x=0,  y=1  }
    Down  -> { x=0,  y=-1 }

vecAdd : Positioned a -> Positioned b -> Positioned b
a `vecAdd` b = { b | x <- a.x + b.x,
                     y <- a.y + b.y }

moveDir : Dir -> Positioned a -> Positioned a
moveDir dir pos = toPositioned dir `vecAdd` pos
