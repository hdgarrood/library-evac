module Geometry where

import Prelude hiding (Left, Right)
import qualified Prelude 
import Control.Arrow

import Types

toVec :: Dir -> Vec
toVec d = Vec $ case d of
    Left  -> (-1, 0)
    Right -> (1, 0)
    Up    -> (0, 1)
    Down  -> (0, -1)

vecAdd :: (VecLike a, VecLike b) => a -> b -> b
a `vecAdd` b = setVec (sum a b) b
    where
    sum = curry $
        (unVec . getVec) *** (unVec . getVec) >>>
        (\((x1, y1), (x2, y2)) -> (x1 + x2, y1 + y2)) >>>
        Vec

moveDir :: VecLike a => Dir -> a -> a
moveDir dir x = toVec dir `vecAdd` x
