module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
-- import Model.Player 

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Brick.Main as M
import qualified Brick.Widgets.Dialog as D

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as Map
import Control.Monad

import qualified Data.String.Utils as U
import Data.List.Split

import Linear.V2 (V2 (..), _x, _y)
import Data.Sequence (Seq (..), (<|))
import System.Random (Random (..), getStdRandom, newStdGen)
import Data.List
import Control.Lens hiding ((:<), (:>), (<|), (|>))

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import System.IO
import Data.Maybe (fromMaybe)

import qualified Data.Sequence as S

-------------------------------------------------------------------------------

funcs :: IORef (Map.Map String Int)
funcs = unsafePerformIO $ newIORef Map.empty

net_insert :: String -> Int -> IO ()
net_insert str d = atomicModifyIORef funcs (\m -> (Map.insert str d m, ()))

-- net_lookup :: IO ()
net_lookup str = do
  fs <- readIORef funcs
  case (Map.lookup str fs) of
    Just x -> return x
    Nothing -> return 0

-- control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = control' (gameState s) s ev


control' 1 g ev = case ev of
  -- AppEvent Tick                   -> Brick.continue =<< liftIO (update s)
  -- AppEvent Tick                   -> Brick.continue (update s (isNetwork s))
  -- T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play X s)
  -- T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  -- T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  -- T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  -- T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  -- T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  AppEvent Tick -> Brick.continue =<< liftIO (update $ eatBonus $ step2 (step g))
  T.VtyEvent (V.EvKey V.KUp []) -> Brick.continue $ turn North g
  T.VtyEvent (V.EvKey V.KDown []) -> Brick.continue $ turn South g
  -- T.VtyEvent (V.EvKey V.KRight []) -> Brick.continue $ turn East g
  -- T.VtyEvent (V.EvKey V.KLeft []) -> Brick.continue $ turn West g
  T.VtyEvent (V.EvKey (V.KChar 'k') []) -> Brick.continue $ turn North g
  T.VtyEvent (V.EvKey (V.KChar 'j') []) -> Brick.continue $ turn South g
  -- T.VtyEvent (V.EvKey (V.KChar 'l') []) -> Brick.continue $ turn East g
  -- T.VtyEvent (V.EvKey (V.KChar 'h') []) -> Brick.continue $ turn West g
  T.VtyEvent (V.EvKey (V.KChar 'r') []) -> Brick.continue =<< liftIO (initGame)
  T.VtyEvent (V.EvKey (V.KChar 'q') []) -> Brick.halt g
  T.VtyEvent (V.EvKey V.KEsc []) -> Brick.halt g
  _                               -> Brick.continue g -- Brick.halt s

control' 0 s (VtyEvent ev) = control0 s ev
control' 2 s (VtyEvent ev) = control2 s ev
control' 3 s (VtyEvent ev) = control3 s ev
control' 4 s (VtyEvent ev) = control4 s ev
control' _ s ev = Brick.continue s

-- | Step forward in time
step2
  g@PS {gameState = gs, score = s} = case (isdie g) of
    True -> g {gameState = 4}
    False -> g {gameState = 1, score = s + 5}
    -- if isdie g
    --   then g & gameState .~ 4 & score .~ s
    --   else move g & gameState .~ 1 & score .~ s + 5

step s = flip execState s . runMaybeT $ do
  -- MaybeT $ guard . not <$> orM [use paused, use dead] -- MaybeT $ guard . not <$> orM [use paused, use dead]
  -- MaybeT . fmap Just $ locked .= False
  generatePillar <|> MaybeT (Just <$> modify move)

update s = case (isNetwork s) of
  0 -> do
    return s
  1 -> do
    _ <- net_insert "bird_self" (bird2int $ bird1 s)
    bird2 <- net_lookup "bird_comp"
    return s {bird2 = int2bird bird2}

control0 d ev = case ev of
  V.EvKey V.KEsc [] -> Brick.halt d
  V.EvKey V.KEnter [] -> do
    nextDialog <- D.handleDialogEvent ev (choices d)
    Brick.continue (getState d nextDialog)
  _ -> do
    nextDialog <- D.handleDialogEvent ev (choices d)
    Brick.continue (d {choices = nextDialog})
control0 d _ = Brick.continue d

getState s d = case (D.dialogSelection d) of
  Just 0 -> s {gameState = 1, isNetwork = 0}
  Just 1 -> s {gameState = 2, isNetwork = 1}
  Nothing -> s {gameState = 0}


control2 d ev = case ev of
  V.EvKey V.KEsc [] -> Brick.halt d
  V.EvKey V.KEnter [] -> do
    nextDialog <- D.handleDialogEvent ev (choices2 d)
    Brick.continue (getState2 d nextDialog)
  _ -> do
    nextDialog <- D.handleDialogEvent ev (choices2 d)
    Brick.continue (d {choices2 = nextDialog})
control2 d _ = Brick.continue d

getState2 s d = case (D.dialogSelection d) of
  Just 0 -> s {gameState = 3, isServer = 1}
  Just 1 -> s {gameState = 3, isServer = 0}
  Nothing -> s {gameState = 2}

control3 s ev =
    case ev of
        V.EvKey V.KEsc [] -> Brick.halt s
        V.EvKey V.KEnter [] -> case F.focusGetCurrent (_focusRing (st s)) of
          Just "Edit1" -> do
            Brick.continue (s {st = (getRingNext (st s) (_focusRing (st s)))})
          Just "Edit2" -> case (isServer s) of 
            0 -> do
              _ <- liftIO $ net_insert "bird_self" 0
              _ <- liftIO $ net_insert "bird_comp" 0
              _ <- liftIO $ forkIO $ runClient
                (U.strip (unlines (E.getEditContents (_edit1 (st s)))))
                (U.strip (unlines (E.getEditContents (_edit2 (st s)))))
              Brick.continue (s {gameState = 1})
            1 -> do
              _ <- liftIO $ net_insert "bird_self" 0
              _ <- liftIO $ net_insert "bird_comp" 0
              _ <- liftIO $ forkIO $ runServer
                (U.strip (unlines (E.getEditContents (_edit1 (st s)))))
                (U.strip (unlines (E.getEditContents (_edit2 (st s)))))
              Brick.continue (s {st2msg = "Waiting for partner to join..."})
        _ -> case F.focusGetCurrent (_focusRing (st s)) of
              Just "Edit1" -> do
                edt <- E.handleEditorEvent ev (_edit1 (st s))
                Brick.continue (s {st = (getEdit1 edt (st s))})
              Just "Edit2" -> do
                edt <- E.handleEditorEvent ev (_edit2 (st s))
                Brick.continue (s {st = (getEdit2 edt (st s))})
              Nothing -> Brick.continue s
control3 s _ = Brick.continue s

control4 s ev = case ev of
  V.EvKey V.KEsc [] -> Brick.halt s
  _ -> Brick.continue s

getEdit1 edt st = st {_edit1 = edt}
getEdit2 edt st = st {_edit2 = edt}
getRingNext st r = st {_focusRing = F.focusNext r}

-- generatePillar :: MaybeT (State Game) ()
generatePillar = do
  -- g <- get
  MaybeT . fmap guard $ (isWall <$> get) --(==) <$> (distanceToWall <$> get) <*> use wall
    -- g <- get
    -- (distanceToWall g) == 0
    -- (==) <$> (distanceToWall <$> get) <*> use wall
  -- g <- get
  -- case (distanceToWall g) of

  MaybeT . fmap Just $ do
    g <- get
    Control.Monad.Trans.State.put g {
      pl1=nextP1 g (pl1 g),
      pl2=nextP2 g (pl2 g),
      pl3=nextP3 g (pl3 g),
      x1=nextX g (x1 g),
      x2=nextX g (x2 g),
      x3=nextX g (x3 g)
    }

    -- get >>= \g -> modifying pl1 (nextP1 g)
    -- get >>= \g -> modifying pl2 (nextP2 g)
    -- get >>= \g -> modifying pl3 (nextP3 g)
    -- get >>= \g -> modifying x1 (nextX g)
    -- get >>= \g -> modifying x2 (nextX g)
    -- get >>= \g -> modifying x3 (nextX g)
    nextRandomPillar
    updateBonus
    

isWall g = (distanceToWall g) == 0

-- generate the length of next pillar3
-- nextB :: Game -> Coord -> Coord
-- nextB g@Game {bonus = bonus, nextBonus = nb} c = nb

-- | Possibly eat bonus if next head position is bonus
-- eatBonus :: Game -> Game
eatBonus g@PS {bird1 = bird1, nextBonus = nb, score = s} = case (isBonus g) of
  True -> g{
    bonus = nb,
    score = s + 100000 --20
  }
  False -> g
--   if isBonus g
--     then g & bonus .~ nb & score .~ (s + 20)
--     else g

-- isBonus :: Game -> Bool
isBonus g@PS {bonus = bonus, bird1 = bird1} = bonus `elem` bird1

-- nextRandomPillar :: State Game ()
nextRandomPillar =
  do
    g <- get
    let (randp :| randps) = (randPs g)
    -- (randp :| randps) <- use randPs
    -- randPs .= randps
    -- g <- get
    let touchWall = x1 g == 0 || x2 g == 0 || x3 g == 0
    if touchWall
      then nextRandomPillar
      else Control.Monad.Trans.State.put g {randP = randp}

updateBonus = do
  g <- get
  let (bo :| bs) = bonusList g
  Control.Monad.Trans.State.put g {
    nextBonus = bo
  }
-- generate the length of next pillar. If current x-coord is 0, it means we've touched the wall
-- so we need to use a new random length `rp`. Else, we remain the same
-- nextP :: Int -> Int -> Int -> Int
nextP x rp p = if x /= 0 then p else rp

-- generate the length of next pillar1
-- nextP1 :: Game -> Int -> Int
nextP1 g@PS {x1 = xx1, x2 = xx2, x3 = xx3, pl1 = ppl1, pl2 = ppl2, pl3 = ppl3, randP = rp} p1 =
  nextP xx1 rp ppl1

-- generate the length of next pillar2
-- nextP2 :: Game -> Int -> Int
nextP2 g@PS {x1 = xx1, x2 = xx2, x3 = xx3, pl1 = ppl1, pl2 = ppl2, pl3 = ppl3, randP = rp} p2 =
  nextP xx2 rp ppl2

-- generate the length of next pillar3
-- nextP3 :: Game -> Int -> Int
nextP3 g@PS {x1 = xx1, x2 = xx2, x3 = xx3, pl1 = ppl1, pl2 = ppl2, pl3 = ppl3, randP = rp} p3 =
  nextP xx3 rp ppl3

--move pillar to the left by decreasing the x-coord
-- nextX :: Game -> Int -> Int
nextX g x = (x -1) `mod` width

-- distanceToWall :: Game -> Int
distanceToWall PS {x1 = xx1, x2 = xx2, x3 = xx3} = minimum [x | x <- [xx1, xx2, xx3]]

-- bird1Y :: Game -> Int
-- bird1Y Game {_bird1 = (V2 x y)} = y

-- writescore::Game -> IO Game
-- writescore g@Game { _dir = d, _bird1 = ((V2 xm ym) :<| _) ,_score=s} = do
--       let x = show s
--       appendFile "/home/cse230/Desktop/test.txt" (x ++ "\n")
--       return g -- TODO create file

-- isdie :: Game -> Bool
isdie g@PS {bird1 = ((V2 xm ym) :<| _), pl1 = pl1, pl2 = pl2, pl3 = pl3, x1 = x1, x2 = x2, x3 = x3}
  | ym == 0 = True
  | ym == height = True
  | iscollision g = True
isdie _ = False

-- TODO collision

-- if ym == 1 || ym==20 then True else False
-- iscollision :: Game -> Bool
iscollision g@PS {bird1 = ((V2 xm ym) :<| _), pl1 = pl1, pl2 = pl2, pl3 = pl3, x1 = x1, x2 = x2, x3 = x3}
  | xm == x1 && (ym `elem` [0 .. pl1] ++ [pl1 + gapSize .. height]) = True
  | xm == x2 && (ym `elem` [0 .. pl2] ++ [pl2 + gapSize .. height]) = True
  | xm == x3 && (ym `elem` [0 .. pl3] ++ [pl3 + gapSize .. height]) = True
iscollision _ = False



-- | Move snake along in a marquee fashion
-- move :: Game -> Game
move g@PS {bird1 = (s :|> _), x1 = xx1, x2 = xx2, x3 = xx3, gameState = l, score = sc} =
  g {
    bird1 = (nextHead g <| s),
    x1 = ((xx1 - 1)) `mod` width,
    x2 = ((xx2 - 1)) `mod` width,
    x3 = ((xx3 - 1)) `mod` width,
    gameState = case (isdie g) of
      True -> 4
      _ -> 1
  }
  -- g & bird1 .~ (nextHead g <| s)
  --   & x1 .~ ((xx1 -1) `mod` width)
  --   & x2 .~ ((xx2 -1) `mod` width)
  --   & x3 .~ ((xx3 -1) `mod` width)
  --   & if isdie g
  --     then gameState .~ 4 -- dead .~ True -- & score .~ sc
  --     else gameState .~ 1 -- dead .~ False
-- & if isdie g TODO
--   then score .~ sc
--   else score .~ sc + 10
move _ = error "Snakes can't be empty!"

-- lowboard :: Game -> Coord
-- lowboard PS {dir = d, bird1 = (a :<| _)}
--   | d == North = a & _y %~ (\y -> height)
--   | d == South = a & _y %~ (\y -> height)
--   -- | d == East = a & _y %~ (\y -> height)
--   -- | d == West = a & _y %~ (\y -> height)
-- lowboard _ = error "Snakes can't be empty!"

-- | Get next head position of the snake
-- nextHead :: Game -> Coord
nextHead PS {dir = d, bird1 = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  -- | d == East = a & y %~ (\y -> (y - 1) `mod` height)
  -- | d == West = a & y %~ (\y -> (y - 1) `mod` height)
nextHead _ = error "Snakes can't be empty!"

-- moveHead :: Game -> Coord
moveHead PS {dir = d, bird1 = (a :<| _)} = a & _y %~ (\y -> y + 4)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
-- turn :: Direction -> Game -> Game
turn d g@PS {bird1 = (s :|> _)} = g {
  bird1 = (moveHead g <| s)
}

-- turn d g = g & dir %~ turnDir d & paused .~ False & locked .~ Trues
-- turn d g = g
-- turn d g = if g ^. locked
--   then g
--   else g & dir %~ turnDir d & paused .~ False & locked .~ True

-- turnDir :: Direction -> Direction -> Direction
-- turnDir n c = c
turnDir n c
  | c `elem` [North, South] && n `elem` [East, West] = n
  | c `elem` [East, West] && n `elem` [North, South] = n
  | otherwise = c

-- drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

bird2int :: Seq Coord -> Int
bird2int ((V2 xm ym) :<| _) = ym

int2bird :: Int -> Seq Coord
int2bird ym = (S.singleton (V2 birdXPos ym))

runServer ip port = do 
  addrinfos <- getAddrInfo Nothing (Just ip) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1

  (conn, _) <- accept sock
  -- _ <- net_insert "bird_y_self" 5
  -- x <- net_lookup "bird_y_self"
  -- print $ show x
  -- print "TCP server is waiting for a message..."
  rrLoop conn


  where
    rrLoop conn = do
      msg <- recv conn 1024
      let m = splitOn "," (C.unpack msg)
      print $ show (C.unpack msg)
      case (C.unpack msg) of
        "exit" -> do
          print "TCP server socket is closing now."
          close conn
          -- close sock
          return ()
        _ -> do
          let n = length m
          net_insert "bird_comp" (read (m !! 0))
          x <- net_lookup "bird_self"
          pl1 <- net_lookup "pl1"
          pl2 <- net_lookup "pl2"
          pl3 <- net_lookup "pl3"
          x1 <- net_lookup "x1"
          x2 <- net_lookup "x2"
          x3 <- net_lookup "x3"
          score <- net_lookup "score"
          -- print x
          let msg = C.pack (show x ++ "," ++ show pl1 ++ "," ++ show pl2 ++ "," ++ show pl3 ++ "," ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show score)
          unless (BS.null msg) $ do
          -- print ("TCP server received: " ++ C.unpack msg)
          -- print "TCP server is now sending a message to the client"
            sendAll conn msg
          rrLoop conn

runClient ip port = do
  addrinfos <- getAddrInfo Nothing (Just ip) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)

  rrLoop sock
  where
    rrLoop sock = forever $ do
      msg <- recv sock 1024
      case (C.unpack msg) of
        "exit" -> do
          print "TCP server socket is closing now."
          close sock
          -- close sock
          return ()
        _ -> do
          let m = splitOn "," (C.unpack msg)
          let n = length m
          x <- net_lookup "bird_self"
          score <- net_lookup "score"
          net_insert "bird_comp" x
          let send_msg = show x ++ "," ++ show score
          sendAll sock $ C.pack send_msg
      -- sendAll sock $ C.pack "exit"
      
      --print ("TCP client received: " ++ C.unpack msg)
          threadDelay 1000000
          rrLoop sock
