module Player where

import Types (..)

unplayer : Player -> Positioned {}
unplayer (Player r) = r
