{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Snake
  ( initGame,
    step,
    turn,
    Game (..),
    Direction (..),
    bonus,
    -- , dead, food, score, snake
    dead,
    score,
    bird1,
    bird2,
    height,
    width,
    -- from C branch
    pl1,
    pl2,
    pl3,
    x1,
    x2,
    x3,
    step2,
    eatBonus,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import System.IO
import System.Random (Random (..), getStdRandom, newStdGen)

-- Types

data Game = Game
  { -- | snake as a sequence of points in N2
    _bird1 :: Bird,
    _bird2 :: Bird,
    -- | direction
    _dir :: Direction,
    -- | location of the food
    -- , _foods  :: Stream Coord -- ^ infinite list of random next food locations
    _bonus :: Coord,
    _nextBonus :: Coord,
    _bonusList :: Stream Coord,
    _isnetwork :: Bool,
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    _locked :: Bool,
    -- | lock to disallow duplicate turns between time steps
    -- from C branch
    _historyscore :: [Int],
    _pl1 :: Int,
    _pl2 :: Int,
    _pl3 :: Int,
    _x1 :: Int,
    _x2 :: Int,
    _x3 :: Int,
    _wall :: Int,
    _randP :: Int,
    _randPs :: Stream Int
  }
  deriving (Show)

type Coord = V2 Int

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

height, width, gapSize, offset :: Int
height = 50
width = 50
gapSize = height * 3 `div` 10
offset = height `div` 6

filename :: String
filename = "test.txt"

-- Functions
split :: String -> [String]
split [] = [""]
split (c : cs)
  | c == '\n' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

-- | Step forward in time
step2 :: Game -> Game
step2
  g@Game {_dead = l, _score = s} =
    if isdie g
      then g & dead .~ True & score .~ s
      else move g & dead .~ l & score .~ s + 5

step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False
  -- generatePillar or move;
  generatePillar <|> MaybeT (Just <$> modify move)


-- newtype Op a = State Game ()

-- liftIO :: IO a -> Op a
-- liftIO io = Op $ \st -> do
--   x <- io
  -- return (st, x)

-- writescore :: Game -> IO ()
-- writescore :: Game -> State Game ()
-- writescore g@Game {_score = s} = 
--   do
--     let x = show s
--     _ <- appendFile filename (x ++ "\n")
--     return g

-- die :: MaybeT (StateT Game m) ()
-- die g = case (isdie g) of
--     False -> g
--     True -> g -- do
        -- return =<< liftIO (writescore g)
      -- True -> do
      --   MaybeT . fmap Just $ do
      --     _ <- writescore g
      --     return g
    -- MaybeT . fmap guard $ (==) <$> (isdie <$> get) <*> use trueFlag
    -- MaybeT . fmap Just $ do
      -- _ <- writescore g
      -- return g


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
    updateBonus
    nextRandomPillar

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

-- bird1Y :: Game -> Int
-- bird1Y Game {_bird1 = (V2 x y)} = y

-- writescore::Game -> IO Game
-- writescore g@Game { _dir = d, _bird1 = ((V2 xm ym) :<| _) ,_score=s} = do
--       let x = show s
--       appendFile "/home/cse230/Desktop/test.txt" (x ++ "\n")
--       return g -- TODO create file

isdie :: Game -> Bool
isdie g@Game {_bird1 = ((V2 xm ym) :<| _), _pl1 = pl1, _pl2 = pl2, _pl3 = pl3, _x1 = x1, _x2 = x2, _x3 = x3}
  | ym == 0 = True
  | ym == height = True
  | iscollision g = True
isdie _ = False

-- collision
iscollision :: Game -> Bool
iscollision g@Game {_bird1 = ((V2 xm ym) :<| _), _pl1 = pl1, _pl2 = pl2, _pl3 = pl3, _x1 = x1, _x2 = x2, _x3 = x3}
  | xm == x1 && (ym `elem` [0 .. pl1] ++ [pl1 + gapSize .. height]) = True
  | xm == x2 && (ym `elem` [0 .. pl2] ++ [pl2 + gapSize .. height]) = True
  | xm == x3 && (ym `elem` [0 .. pl3] ++ [pl3 + gapSize .. height]) = True
iscollision _ = False

-- generate the length of next pillar3
nextB :: Game -> Coord -> Coord
nextB g@Game {_bonus = bonus, _nextBonus = nb} c = nb

-- | Possibly eat bonus if next head position is bonus
eatBonus :: Game -> Game
eatBonus g@Game {_bird1 = bird1, _nextBonus = nb, _score = s} =
  if isBonus g
    then g & bonus .~ nb & score .~ (s + 20)
    else g

-- eatBonus :: MaybeT (State Game) ()
-- eatBonus = do
--   MaybeT . fmap guard $ (isBonus <$> get)
--   MaybeT . fmap Just $ do
--     modifying score (+ 20)
--     nextBonus

-- get >>= \g -> step2(step) g

-- $ (==) <$> (nexthead <$> get) <*> use bonus
--    get >>= \g -> modifying bonus (nextBonus g)

-- TODO: find a way to call this

-- | Set a valid next food coordinate
updateBonus :: State Game ()
updateBonus = do
  (bo :| bs) <- use bonusList
  bonusList .= bs
  nextBonus .= bo

isBonus :: Game -> Bool
isBonus g@Game {_bonus = bonus, _bird1 = bird1} = bonus `elem` bird1

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game {_bird1 = (s :|> _), _x1 = xx1, _x2 = xx2, _x3 = xx3, _dead = l, _score = sc} =
  g & bird1 .~ (nextHead g <| s)
    & x1 .~ ((xx1 -1) `mod` width)
    & x2 .~ ((xx2 -1) `mod` width)
    & x3 .~ ((xx3 -1) `mod` width)
    & if isdie g
      then dead .~ True -- & score .~ sc
      else dead .~ False
-- & if isdie g TODO
--   then score .~ sc
--   else score .~ sc + 10
move _ = error "Snakes can't be empty!"

lowboard :: Game -> Coord
lowboard Game {_dir = d, _bird1 = (a :<| _)}
  | d == North = a & _y %~ (\y -> height)
  | d == South = a & _y %~ (\y -> height)
  | d == East = a & _y %~ (\y -> height)
  | d == West = a & _y %~ (\y -> height)
lowboard _ = error "Snakes can't be empty!"

-- | Get next head position of the snake
nextHead :: Game -> Coord
nextHead Game {_dir = d, _bird1 = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == West = a & _y %~ (\y -> (y - 1) `mod` height)
nextHead _ = error "Snakes can't be empty!"

moveHead :: Game -> Coord
moveHead Game {_dir = d, _bird1 = (a :<| _)} = a & _y %~ (\y -> y + 4)

currHead :: Game -> Coord
currHead Game {_bird1 = (a :<| _)} = a & _y %~ id

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g@Game {_bird1 = (s :|> _)} = g & bird1 .~ (moveHead g <| s)

-- turn d g = g & dir %~ turnDir d & paused .~ False & locked .~ Trues
-- turn d g = g
-- turn d g = if g ^. locked
--   then g
--   else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
-- turnDir n c = c
turnDir n c
  | c `elem` [North, South] && n `elem` [East, West] = n
  | c `elem` [East, West] && n `elem` [North, South] = n
  | otherwise = c

addscorelist :: Game -> [Int] -> Game
addscorelist
  g@Game
    { _historyscore = old
    }
  h = g & historyscore .~ h



drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))


writescore :: Game -> IO Game
writescore g@Game {_score = s} =
  do
    let x = show s
    _ <- appendFile filename (x ++ "\n")
    return g

sortDes :: [Int] -> [Int]
sortDes =  sortBy (flip compare)

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  -- contents <- readFile "/home/cse230/Desktop/test.txt"
  (bo :| bs) <-
    fromList . randomRs (V2 (width `div` 4) (height `div` 4), V2 (width `div` 4) (height * 3 `div` 4)) <$> newStdGen
  -- streaming of random pillar length
  contents <- readFile filename
    
  (randp :| randps) <-
    fromList . randomRs (0 + offset, (height `div` 3) + offset) <$> newStdGen
  -- hard code initial pillar length
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)
  let xm = width `div` 4
      ym = height `div` 2
      bonusx = 15
      bonusy = 15
      x = init $ split contents     
      y = sortDes [read a :: Int | a <- x]
      result = take 5 y
      g =
        Game
          { _bird1 = S.singleton (V2 xm ym),
            _bird2 = S.singleton (V2 xm ym),
            _bonus = V2 (height `div` 4) bonusy,
            _nextBonus = bo,
            _bonusList = bs,
            _score = 0,
            _dir = South,
            _dead = False,
            _paused = True,
            _locked = False,
            _isnetwork = False,
            _historyscore = result,
            -- from C branch
            _randP = randp,
            _randPs = randps,
            _pl1 = a,
            _pl2 = b,
            _pl3 = c,
            _x1 = width - 1,
            _x2 = width * 2 `div` 3,
            _x3 = width `div` 3,
            _wall = 0
          }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
