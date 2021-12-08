{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Snake
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  ,food
  -- , dead, food, score, snake
  , dead,  score, bird1, bird2
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _bird1  :: Bird  -- ^ snake as a sequence of points in N2
  , _bird2  :: Bird
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of the food
  -- , _foods  :: Stream Coord -- ^ infinite list of random next food locations
  , _isnetwork :: Bool
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

type Bird = Seq Coord

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

height, width :: Int
height = 20
width = 20

-- Functions

-- | Step forward in time
step :: Game -> Game
step g@Game { _bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=l, _paused=p,_score=s,_locked=m ,_food=f} = if isdie g == True
                                                                          then Game{_bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=True, _paused=p,_score=s,_locked=m,_food=f}
                                                                          else move Game { _bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=l, _paused=p,_score=s+10,_locked=m ,_food=f}                                                                
-- step s = move 
-- -- step s = flip execState s . runMaybeT $ do

--   -- Make sure the game isn't paused or over
--   MaybeT $ guard . not <$> orM [use paused, use dead]

--   -- Unlock from last directional turn
--   -- MaybeT . fmap Just $ locked .= True

--   -- die (moved into boundary), eat (moved into food), or move (move into space)
--   -- die <|> eatFood <|> MaybeT (Just <$> modify move)
--   die  <|> MaybeT (Just <$> modify move)

-- | Possibly die if next head position is in snake
-- die :: MaybeT (State Game) ()
-- die = do
--   MaybeT . fmap guard $ elem <$> (lowboard <$> get) <*> (use _bird1)
--   MaybeT . fmap Just $ dead .= True


isdie :: Game -> Bool
isdie g@Game { _dir = d, _bird1 = ((V2 xm ym) :<| _) } = if ym == 1 || ym==20
                                                         then True
                                                         else False
-- iscollision :: Game -> Bool
-- iscollision 


-- ScoreModify::


-- | Possibly eat food if next head position is food
-- eatFood :: MaybeT (State Game) ()
-- eatFood = do
--   MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use food)
--   MaybeT . fmap Just $ do
--     modifying score (+ 10)
--     get >>= \g -> modifying snake (nextHead g <|)
--     nextFood

-- | Set a valid next food coordinate
-- nextFood :: State Game ()
-- nextFood = do
--   (f :| fs) <- use foods
--   foods .= fs
--   elem f <$> use snake >>= \case
--     True -> nextFood
--     False -> food .= f

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game { _bird1 = (s :|> _) } = g & bird1 .~ (nextHead g <| s)
move _                             = error "Snakes can't be empty!"



lowboard :: Game -> Coord
lowboard Game { _dir = d, _bird1 = (a :<| _) } 
  | d == North = a & _y %~ (\y -> height) 
  | d == South = a & _y %~ (\y -> height)
  | d == East  = a & _y %~ (\y -> height)
  | d == West  = a & _y %~ (\y -> height)
lowboard _ = error "Snakes can't be empty!"

-- | Get next head position of the snake
nextHead :: Game -> Coord
nextHead Game { _dir = d, _bird1 = (a :<| _) } 
  | d == North = a & _y %~ (\y -> (y - 1) `mod` height) 
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == West  = a & _y %~ (\y -> (y - 1) `mod` height)
nextHead _ = error "Snakes can't be empty!"


-- pillarnextHead :: Game -> Coord
-- pillarnextHead Game { _dir = d, _pillar = (a :<| _) } 
--   | d == North = a & _x %~ (\x -> (x - 1) `mod` width) 
--   | d == South = a & _x %~ (\x -> (x - 1) `mod` width)
--   | d == East  = a & _x %~ (\x -> (x - 1) `mod` width)
--   | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)
-- pillarnextHead _ = error "Snakes can't be empty!"


moveHead :: Game -> Coord
moveHead Game { _dir = d, _bird1 = (a :<| _) } = a & _y %~ (\y -> (y + 4) )



-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g@Game { _bird1 = (s :|> _) } = g & bird1 .~ (moveHead g <| s)
-- turn d g = g & dir %~ turnDir d & paused .~ False & locked .~ Trues
-- turn d g = g 
-- turn d g = if g ^. locked
--   then g
--   else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
-- turnDir n c = c
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      bonusx = 15
      bonusy = 15
      g  = Game
        { _bird1  = (S.singleton (V2 xm ym)),
          _bird2 = (S.singleton (V2 xm ym))
        , _food   = (V2 bonusx bonusy)
        -- , _foods  = fs
        , _score  = 0
        , _dir    = South
        , _dead   = False
        , _paused = True
        , _locked = False
        ,_isnetwork = False
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
