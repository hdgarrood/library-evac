{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Types where

import Control.Arrow ((***))
import Data.Map (Map)
import Data.String (IsString)
import Data.Time.Clock (DiffTime)
import Control.Monad.Random (Rand)
import Control.Monad.Trans.Reader (ReaderT)
import System.Random (StdGen)

data Zipper a = Z [a] a [a]
    deriving (Show, Eq)

instance Functor Zipper where
    fmap f (Z preds x succs) = Z (map f preds) (f x) (map f succs)

class VecLike a where
    getVec :: a -> Vec
    setVec :: Vec -> a -> a

    getX :: a -> Int
    getX = fst . getVec

    getY :: a -> Int
    getY = snd . getVec

type Vec = (Int, Int)

instance VecLike Vec where
    getVec = id
    setVec = const

data Player = Player { unPlayer :: Vec }
    deriving (Show, Eq)

instance VecLike Player where
    getVec = unPlayer
    setVec v = const . Player $ v

data Dir = Left | Right | Up | Down
    deriving (Show, Eq, Ord)
data FloorTile = Normal | Inaccessible | Stairs StairsDir
    deriving (Show, Eq)
data StairsDir = Upwards | Downwards
    deriving (Show, Eq)

data Object = forall s.
    Object { objectPosition :: (Int, Int)
           , objectType :: ObjectType
           , objectPerformStep :: s -> GameM ([Action], s)
           , objectState :: s
           }

data ObjectType = Fire | Person
    deriving (Show, Eq, Ord)
data FireIntensity = I1 | I2 | I3 | I4 | I5
    deriving (Show, Eq, Ord)

-- A square adjacent to a thing.
data Adjacent = O -- ^ On top of
                | N | NE | E | SE | S | SW | W | NW
    deriving (Show, Eq, Ord)

-- Something an object can ask to do during a step.
data Action = Move Dir -- ^ Move in a given direction.
            | GrowFire Adjacent -- ^ Start or increase intensity of a fire.
    deriving (Show, Eq, Ord)

data Floor = Floor { floorTiles :: [[FloorTile]]
                   , floorObjects :: Map ObjectId Object
                   , floorLastObjectId :: ObjectId
                   }

data FloorCollection = FloorCollection
    { collectionFloors :: Map FloorId Floor
    , collectionZipper :: Zipper FloorId
    }

newtype ObjectId = ObjectId Int
    deriving (Show, Eq, Ord)
newtype FloorId = FloorId String
    deriving (Show, Eq, Ord, IsString)

data Transition = Transition
    { transitionReason :: TransitionReason
    , transitionPerformed :: Bool
    -- ^ at some point during the transition, the action it's responsible for
    -- performing (eg changing floors) happens instantaneously. This flag
    -- indicates whether that has happened yet.
    , transitionRemaining :: DiffTime
    }
    deriving (Show, Eq)

data TransitionReason = UsingStairs StairsDir
    deriving (Show, Eq)

data GameState = GameState
    { stateFloors :: FloorCollection
    , statePlayer :: Player
    , stateTransition :: Maybe Transition
    , stateStdGen :: StdGen
    }

data GameInput = MovePlayer Dir | TimeStep DiffTime | SetRandomSeed Int
    deriving (Show, Eq)

data GameReader = GameReader
    { readerState :: GameState
    , readerInput :: GameInput
    }

newtype GameM a = GameM
    { unGameM :: ReaderT GameReader (Rand Int) a
    }
