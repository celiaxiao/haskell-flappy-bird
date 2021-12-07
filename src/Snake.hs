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
    score,
    -- snake,
    height,
    width,
    pl1,
    pl2,
    pl3,
    x1,
    x2,
    x3,
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
import System.Random (Random (..), getStdRandom, newStdGen)

-- Types

data Game = Game
  { -- | snake as a sequence of points in N2
    -- _snake :: Snake,
    _randP :: Int,
    -- | infinite list of random next food locations
    -- | direction
    _randPs :: Stream Int,
    _dir :: Direction,
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | lock to disallow duplicate turns between time steps
    _locked :: Bool,
    _pl1 :: Int,
    _pl2 :: Int,
    _pl3 :: Int,
    _x1 :: Int,
    _x2 :: Int,
    _x3 :: Int,
    _wall :: Int
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
height, width, gapSize, offset :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6

-- wall = 0
-- pillarLength = height `div` 3

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False
  -- TODO: generatePillar or move; without generatePillar the 5th pillar is not random. With generatePillar we can't move
  generatePillar <|> MaybeT (Just <$> modify move)

-- check whether one of the pillar has reached the wall, if so reset the pillar length for that pillar
generatePillar :: MaybeT (State Game) ()
generatePillar = do
  MaybeT . fmap guard $ (==) <$> (distanceToWall <$> get) <*> use wall
  MaybeT . fmap Just $ do
    get >>= \g -> modifying pl1 (nextP1 g)
    get >>= \g -> modifying pl2 (nextP2 g)
    get >>= \g -> modifying pl3 (nextP3 g)
    get >>= \g -> modifying x1 (nextX g)
    get >>= \g -> modifying x2 (nextX g)
    get >>= \g -> modifying x3 (nextX g)
    nextRandomPillar

-- control the streaming that keeps generating pillar length
nextRandomPillar :: State Game ()
nextRandomPillar =
  do
    (randp :| randps) <- use randPs
    randPs .= randps
    g <- get
    let touchWall = _x1 g == 0 || _x2 g == 0 || _x3 g == 0
    if touchWall
      then nextRandomPillar
      else randP .= randp

-- | Move pillar to the left
move :: Game -> Game
move g@Game {_x1 = xx1, _x2 = xx2, _x3 = xx3} =
  g & x1 .~ ((xx1 -1) `mod` width) & x2 .~ ((xx2 -1) `mod` width) & x3 .~ ((xx3 -1) `mod` width)

-- generate the length of next pillar. If current x-coord is 0, it means we've touched the wall
-- so we need to use a new random length `rp`. Else, we remain the same
nextP :: Int -> Int -> Int -> Int
nextP x rp p = if x /= 0 then p else rp

-- generate the length of next pillar1
nextP1 :: Game -> Int -> Int
nextP1 g@Game {_x1 = xx1, _x2 = xx2, _x3 = xx3, _pl1 = ppl1, _pl2 = ppl2, _pl3 = ppl3, _randP = rp} p1 =
  nextP xx1 rp ppl1

-- generate the length of next pillar2
nextP2 :: Game -> Int -> Int
nextP2 g@Game {_x1 = xx1, _x2 = xx2, _x3 = xx3, _pl1 = ppl1, _pl2 = ppl2, _pl3 = ppl3, _randP = rp} p2 =
  nextP xx2 rp ppl2

-- generate the length of next pillar3
nextP3 :: Game -> Int -> Int
nextP3 g@Game {_x1 = xx1, _x2 = xx2, _x3 = xx3, _pl1 = ppl1, _pl2 = ppl2, _pl3 = ppl3, _randP = rp} p3 =
  nextP xx3 rp ppl3

--move pillar to the left by decreasing the x-coord
nextX :: Game -> Int -> Int
nextX g x = (x -1) `mod` width

distanceToWall :: Game -> Int
distanceToWall Game {_x1 = xx1, _x2 = xx2, _x3 = xx3} = minimum [x | x <- [xx1, xx2, xx3]]

--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g =
  if g ^. locked
    then g
    else g & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c = West

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  -- streaming of random pillar length
  (randp :| randps) <-
    fromList . randomRs (0 + offset, (height `div` 3) + offset) <$> newStdGen
  -- hard code initial pillar length
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)

  let xm = width `div` 2
      ym = height - 1
      pillarLength = height `div` 3
      g =
        Game
          { _score = 0,
            _dir = West,
            _randP = randp,
            _randPs = randps,
            _dead = False,
            _paused = True,
            _locked = False,
            _pl1 = a,
            _pl2 = b,
            _pl3 = c,
            _x1 = width - 1,
            _x2 = width * 2 `div` 3,
            _x3 = width `div` 3,
            _wall = 0
          }
  return $ execState nextRandomPillar g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
