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
  -- from C branch
  ,pl1
  ,pl2
  ,pl3
  ,x1
  ,x2
  ,x3
  ,step2
  ) where
import System.IO
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.List
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random (..), getStdRandom, newStdGen)

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
  , _locked :: Bool   
  , _historyscore :: [Integer]      -- ^ lock to disallow duplicate turns between time steps
  -- from C branch
  , _pl1 :: Int
  , _pl2 :: Int
  , _pl3 :: Int
  , _x1 :: Int
  , _x2 :: Int
  , _x3 :: Int
  , _wall :: Int
  , _randP :: Int
  , _randPs :: Stream Int
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

height, width, gapSize, offset :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6

-- Functions
split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == '\n' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where rest = split cs
    
-- | Step forward in time
step2 :: Game -> Game
step2
  g@Game
    { _dead = l,
      _score = s,
      -- the rest
      _bird1 = a,
      _bird2 = b,
      _isnetwork = net,
      _dir = d,
      _paused = p,
      _locked = m,
      _food = f,
      _randP = rp,
      _randPs = rps,
      _pl1 = pl1,
      _pl2 = pl2,
      _pl3 = pl3,
      _x1 = x1,
      _x2 = x2,
      _x3 = x3,
      _wall = w, 
      _historyscore = h
    } =
    if isdie g
      then
        Game
          { _dead = True,
            _score = s,
            -- the rest
            _bird1 = a,
            _bird2 = b,
            _isnetwork = net,
            _dir = d,
            _paused = p,
            _locked = m,
            _food = f,
            _randP = rp,
            _randPs = rps,
            _pl1 = pl1,
            _pl2 = pl2,
            _pl3 = pl3,
            _x1 = x1,
            _x2 = x2,
            _x3 = x3,
            _wall = w,
            _historyscore = h
          }
      else
        move
          Game
            { _dead = l,
              _score = s + 5,
              -- the rest
              _bird1 = a,
              _bird2 = b,
              _isnetwork = net,
              _dir = d,
              _paused = p,
              _locked = m,
              _food = f,
              _randP = rp,
              _randPs = rps,
              _pl1 = pl1,
              _pl2 = pl2,
              _pl3 = pl3,
              _x1 = x1,
              _x2 = x2,
              _x3 = x3,
              _wall = w,
              _historyscore = h
            }

step :: Game -> Game
step s = flip execState s . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False
  -- TODO: generatePillar or move; without generatePillar the 5th pillar is not random. With generatePillar we can't move
  generatePillar <|> MaybeT (Just <$> modify move)

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

-- writescore::Game -> IO Game
-- writescore g@Game { _dir = d, _bird1 = ((V2 xm ym) :<| _) ,_score=s} = do
--       let x = show s
--       appendFile "/home/cse230/Desktop/test.txt" (x ++ "\n")
--       return g -- TODO create file


isdie :: Game -> Bool
isdie g@Game { _dir = d, _bird1 = ((V2 xm ym) :<| _) ,_score=s} = if ym == 0 || ym==height then True else False
-- TODO collision


-- if ym == 1 || ym==20 then True else False
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

addscorelist :: Game -> [Integer] -> Game
addscorelist g@Game{ _bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=l, _paused=p,_score=s,_locked=m ,_food=f,_historyscore = old,
      _randP = rp,
      _randPs = rps,
      _pl1 = pl1,
      _pl2 = pl2,
      _pl3 = pl3,
      _x1 = x1,
      _x2 = x2, 
      _x3 = x3,
      _wall = w} h = 
  Game
    { _bird1 = a,
      _bird2 = b,
      _isnetwork = net,
      _dir = d,
      _dead = l,
      _paused = p,
      _score = s,
      _locked = m,
      _food = f,
      _historyscore = h,
      _randP = rp,
      _randPs = rps,
      _pl1 = pl1,
      _pl2 = pl2,
      _pl3 = pl3,
      _x1 = x1,
      _x2 = x2,
      _x3 = x3,
      _wall = w
    }


drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

-- | Initialize a paused game with random food location
initGame ::  IO Game
initGame = do
  -- contents <- readFile "/home/cse230/Desktop/test.txt"
  -- (f :| fs) <-
  --   fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
    -- streaming of random pillar length
  (randp :| randps) <-
    fromList . randomRs (0 + offset, (height `div` 3) + offset) <$> newStdGen
  -- hard code initial pillar length
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)
  let xm = width `div` 2
      ym = height `div` 2
      bonusx = 15
      bonusy = 15
      -- x = init $ split contents
      -- y = sort [ read a::Integer | a <-x]
      -- result = take 5 y
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
        ,_historyscore = [] -- result TODO
        -- from C branch
        , _randP = randp
        , _randPs = randps
        , _pl1 = a
        , _pl2 = b
        , _pl3 = c
        , _x1 = width - 1
        , _x2 = width * 2 `div` 3
        , _x3 = width `div` 3
        , _wall = 0
        }
  return g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
