{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Snake
  ( initGame,
    step,
    turn,
    Game (..),
    Direction (..),
    dead,
    food,
    score,
    snake,
    height,
    width,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), iterateN, mapWithIndex, (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.Random (Random (..), newStdGen)

-- Types

data Game = Game
  { -- | snake as a sequence of points in N2
    _snake :: Snake,
    -- | direction
    _dir :: Direction,
    -- | location of the food
    _food :: Coord,
    -- | infinite list of random next food locations
    _foods :: Stream Coord,
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool
  }
  deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

-- TODO: height and width should be 100
height, width, pillarLength :: Int
height = 30
width = 30
pillarLength = height `div` 3

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  die <|> eatFood <|> MaybeT (Just <$> modify move)

-- | Possibly die if next head position is in snake
die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use snake)
  MaybeT . fmap Just $ dead .= True

-- | Possibly eat food if next head position is food
eatFood :: MaybeT (State Game) ()
eatFood = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use food)
  MaybeT . fmap Just $ do
    modifying score (+ 10)
    get >>= \g -> modifying snake (nextHead g <|)
    nextFood

-- | Set a valid next food coordinate
nextFood :: State Game ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs
  elem f <$> use snake >>= \case
    True -> nextFood
    False -> food .= f

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game {_snake = s} = g & snake .~ S.mapWithIndex nextCell s
move _ = error "Snakes can't be empty!"

nextCell :: Int -> Coord -> Coord
nextCell a (V2 x y) = V2 ((x - 1) `mod` width) y

-- g & snake .~ (nextHead g <| s)

-- | Get next head position of the snake
nextHead :: Game -> Coord
nextHead Game {_dir = d, _snake = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead _ = error "Snakes can't be empty!"

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g =
  if g ^. locked
    then g
    else g & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c = West

-- | c `elem` [North, South] && n `elem` [East, West] = n
-- | c `elem` [East, West] && n `elem` [North, South] = n
-- | otherwise = c
initPillar :: [Coord] -> Seq Coord
initPillar = S.fromList

singlePillar :: Int -> Int -> [Coord]
singlePillar pillarLen x = [V2 x (height - 1 - i) | i <- [0 .. pillarLen]]

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height - 1
      g =
        Game
          { _snake = initPillar (singlePillar pillarLength (width - 1)),
            _food = f,
            _foods = fs,
            _score = 0,
            _dir = West,
            _dead = False,
            _paused = True,
            _locked = False
          }
  return $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
