module Control where

import Brick hiding (Result)
-- import Model.Player

import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad
import Control.Monad (guard, unless)
import Control.Monad.Extra (orM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import qualified Data.String.Utils as U
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..), _x, _y)
import Model
import Model.Board
import Model.Player
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import System.IO.Unsafe
import System.Random (Random (..), getStdRandom, newStdGen)

-------------------------------------------------------------------------------

funcs :: IORef (Map.Map String Int)
funcs = unsafePerformIO $ newIORef Map.empty

net_insert :: String -> Int -> IO ()
net_insert str d = atomicModifyIORef funcs (\m -> (Map.insert str d m, ()))

-- net_lookup :: IO ()
net_lookup str = do
  fs <- readIORef funcs
  case Map.lookup str fs of
    Just x -> do
      return x
    Nothing -> return 0

-- control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s = control' (gameState s) s

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
  T.VtyEvent (V.EvKey (V.KChar 'r') []) -> Brick.continue =<< liftIO initGame
  T.VtyEvent (V.EvKey (V.KChar 's') []) -> Brick.continue =<< liftIO (writescore g)
  T.VtyEvent (V.EvKey (V.KChar 'q') []) -> Brick.halt g
  T.VtyEvent (V.EvKey V.KEsc []) -> Brick.halt g
  _ -> Brick.continue g -- Brick.halt s
control' 0 s (VtyEvent ev) = control0 s ev
control' 2 s ev = control2 s ev
control' 3 s ev = control3 s ev
control' 4 s ev = control4 s ev
control' _ s _ = Brick.continue s

-- | Step forward in time
step2
  g@PS {score = s} = if isdie g then g {gameState = 4, self_win = 1} else g {gameState = 1, score = s + 5}

-- if isdie g
--   then g & gameState .~ 4 & score .~ s
--   else move g & gameState .~ 1 & score .~ s + 5

step s = flip execState s . runMaybeT $ do
  -- MaybeT $ guard . not <$> orM [use paused, use dead] -- MaybeT $ guard . not <$> orM [use paused, use dead]
  -- MaybeT . fmap Just $ locked .= False
  shouldUpdate <|> generatePillar <|> MaybeT (Just <$> modify move)

update s = case (isNetwork s) of
  0 -> do
    -- print "not network"
    return s
  1 -> do
    wait <- net_lookup "wait"
    case wait of
      0 -> return s
      1 -> do 
        net_insert "bird_self" (bird2int $ bird1 s)
        case isServer s of
          1 -> do
            bird2 <- net_lookup "bird_comp"
            gs <- net_lookup "gameState"
            net_insert "self_win" (self_win s)
            comp_win <- net_lookup "comp_win"
            case (gameState s) of
              3 -> do
                return ( s{
                  gameState = gs
                })
              1 -> do 
                net_insert "pl1" (pl1 s)
                net_insert "pl2" (pl2 s)
                net_insert "pl3" (pl3 s)
                net_insert "x1" (x1 s)
                net_insert "x2" (x2 s)
                net_insert "x3" (x3 s)
                return (s {
                  bird2 = int2bird bird2,
                  comp_win = comp_win,
                  gameState = case comp_win of
                    1 -> 4
                    0 -> 1
                })
              4 -> do
                return (s {
                  comp_win = comp_win
                })
          0 -> do
            bird2 <- net_lookup "bird_comp"
            pl1 <- net_lookup "pl1"
            pl2 <- net_lookup "pl2"
            pl3 <- net_lookup "pl3"
            x1 <- net_lookup "x1"
            x2 <- net_lookup "x2"
            x3 <- net_lookup "x3"
            net_insert "self_win" (self_win s)
            comp_win <- net_lookup "comp_win"
            case (gameState s) of
              1 -> do
                return (s {
                  bird2 = int2bird bird2,
                  pl1 = pl1,
                  pl2 = pl2,
                  pl3 = pl3,
                  x1 = x1,
                  x2 = x2,
                  x3 = x3,
                  comp_win = comp_win,
                  gameState = case comp_win of
                    1 -> 4
                    0 -> 1
                })
              4 -> do
                return (s {
                  comp_win = comp_win
                })

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

control2 d ev@(T.VtyEvent evt) = case ev of
  AppEvent Tick -> Brick.continue =<< liftIO (update d)
  T.VtyEvent (V.EvKey V.KEsc []) -> Brick.halt d
  T.VtyEvent (V.EvKey V.KEnter []) -> do
    nextDialog <- D.handleDialogEvent evt (choices2 d)
    Brick.continue (getState2 d nextDialog)
  _ -> do
    nextDialog <- D.handleDialogEvent evt (choices2 d)
    Brick.continue (d {choices2 = nextDialog})

control2 d _ = Brick.continue d

getState2 s d = case D.dialogSelection d of
  Just 0 -> s {gameState = 3, isServer = 1}
  Just 1 -> s {gameState = 3, isServer = 0}
  Nothing -> s {gameState = 2}

control3 s (T.VtyEvent ev) =
  case ev of
    -- AppEvent Tick -> Brick.continue =<< liftIO (update s)
    V.EvKey V.KEsc [] -> Brick.halt s
    V.EvKey V.KEnter [] -> case F.focusGetCurrent (_focusRing (st s)) of
      Just "Edit1" -> do
        Brick.continue (s {st = (getRingNext (st s) (_focusRing (st s)))})
      Just "Edit2" -> case isServer s of
        0 -> do
          _ <- liftIO $ net_insert "bird_self" 0
          _ <- liftIO $ net_insert "bird_comp" 0
          _ <- liftIO $ net_insert "wait" 1
          _ <- liftIO $ net_insert "self_win" 0
          _ <- liftIO $ net_insert "comp_win" 0
          _ <-
            liftIO $
              forkIO $
                runClient
                  (U.strip (unlines (E.getEditContents (_edit1 (st s)))))
                  (U.strip (unlines (E.getEditContents (_edit2 (st s)))))
          Brick.continue (s {gameState = 1})
        1 -> do
          _ <- liftIO $ net_insert "bird_self" 0
          _ <- liftIO $ net_insert "bird_comp" 0
          _ <- liftIO $ net_insert "gameState" 3
          _ <- liftIO $ net_insert "x1" (x1 s)
          _ <- liftIO $ net_insert "x2" (x2 s)
          _ <- liftIO $ net_insert "x3" (x3 s)
          _ <- liftIO $ net_insert "pl1" (pl1 s)
          _ <- liftIO $ net_insert "pl2" (pl2 s)
          _ <- liftIO $ net_insert "pl3" (pl3 s)
          _ <- liftIO $ net_insert "wait" 1
          _ <- liftIO $ net_insert "self_win" 0
          _ <- liftIO $ net_insert "comp_win" 0
          _ <-
            liftIO $
              forkIO $
                runServer
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
control3 s (AppEvent Tick) = Brick.continue =<< liftIO (update s)
control3 s _ = Brick.continue s


control4 s evt@(VtyEvent ev) = case ev of
  V.EvKey V.KEsc [] -> Brick.halt s
  (V.EvKey (V.KChar 'r') []) -> Brick.continue =<< liftIO initGame
  (V.EvKey (V.KChar 's') []) -> Brick.continue =<< liftIO (writescore s)
  (V.EvKey (V.KChar 'q') []) -> Brick.halt s
  _ -> Brick.continue s
control4 s (AppEvent Tick) = Brick.continue =<< liftIO (update s)
control4 s _ = Brick.continue s

getEdit1 edt st = st {_edit1 = edt}

getEdit2 edt st = st {_edit2 = edt}

getRingNext st r = st {_focusRing = F.focusNext r}

shouldUpdate :: MaybeT (StateT PlayState Identity) ()
shouldUpdate = do
  MaybeT . fmap guard $ (isClient <$> get)
  MaybeT . fmap Just $ do
    g <- get
    let (s :|> _) = bird1 g
    Control.Monad.Trans.State.put (g{
      bird1 = (nextHead g <| s),
      gameState = if isdie g then 4 else 1,
      self_win = if isdie g then 1 else 0
    })

isClient :: PlayState -> Bool
isClient PS {isNetwork = isn, isServer = iss} = isn == 1 && iss == 0

-- generatePillar :: MaybeT (State Game) ()
generatePillar :: MaybeT (StateT PlayState Identity) ()
generatePillar = do
  MaybeT . fmap guard $ (isWall <$> get)
  MaybeT . fmap Just $ do
    g <- get
    Control.Monad.Trans.State.put
      g
        { pl1 = nextP1 g,
          pl2 = nextP2 g,
          pl3 = nextP3 g,
          x1 = nextX g (x1 g),
          x2 = nextX g (x2 g),
          x3 = nextX g (x3 g)
        }
    nextRandomPillar
    updateBonus

isWall g = distanceToWall g == 0

-- generate the length of next pillar3
-- nextB :: Game -> Coord -> Coord
-- nextB g@Game {bonus = bonus, nextBonus = nb} c = nb

-- | Possibly eat bonus if next head position is bonus
-- eatBonus :: Game -> Game
eatBonus g@PS {nextBonus = nb, score = s, bonusList = bl} =
  if isBonus g
    then
      g
        { bonus = V2 birdXPos (bl !! nb),
          score = s + 20 --20
        }
    else g

--   if isBonus g
--     then g & bonus .~ nb & score .~ (s + 20)
--     else g

-- isBonus :: Game -> Bool
isBonus PS {bonus = bonus, bird1 = bird1} = bonus `elem` bird1

-- nextRandomPillar :: State Game ()
nextRandomPillar = do
  g <- get
  Control.Monad.Trans.State.put g {randP = (randP g + 1) `mod` pListLen}

updateBonus = do
  g <- get
  Control.Monad.Trans.State.put g {nextBonus = (nextBonus g + 1) `mod` bListLen}

-- generate the length of next pillar. If current x-coord is 0, it means we've touched the wall
-- so we need to use a new random length `rp`. Else, we remain the same
-- nextP :: Int -> Int -> Int -> Int
nextP x rp p = if x /= 0 then p else rp

-- generate the length of next pillar1
-- nextP1 :: Game -> Int -> Int
nextP1 PS {x1 = xx1, pl1 = ppl1, randP = rp, randPs = rps} =
  nextP xx1 (rps !! rp) ppl1

-- generate the length of next pillar2
-- nextP2 :: Game -> Int -> Int
nextP2 PS {x2 = xx2, pl2 = ppl2, randP = rp} =
  nextP xx2 rp ppl2

-- generate the length of next pillar3
-- nextP3 :: Game -> Int -> Int
nextP3 PS {x3 = xx3, pl3 = ppl3, randP = rp} =
  nextP xx3 rp ppl3

--move pillar to the left by decreasing the x-coord
-- nextX :: Game -> Int -> Int
nextX g x = (x -1) `mod` width

-- distanceToWall :: Game -> Int
distanceToWall PS {x1 = xx1, x2 = xx2, x3 = xx3} = minimum [x | x <- [xx1, xx2, xx3]]

isdie g@PS {bird1 = ((V2 xm ym) :<| _)}
  | ym == 0 = True
  | ym == height = True
  | iscollision g = True
isdie _ = False

-- collision

-- if ym == 1 || ym==20 then True else False
-- iscollision :: Game -> Bool
iscollision g@PS {bird1 = ((V2 xm ym) :<| _), pl1 = pl1, pl2 = pl2, pl3 = pl3, x1 = x1, x2 = x2, x3 = x3}
  | collide xm ym x1 pl1 = True
  | collide xm ym x2 pl2 = True
  | collide xm ym x3 pl3 = True
iscollision _ = False

collide :: Int -> Int -> Int -> Int -> Bool
collide bx by px py = bx == px && (by `elem` [0 .. py] ++ [py + gapSize .. height])

-- move :: Game -> Game
move g@PS {bird1 = (s :|> _), x1 = xx1, x2 = xx2, x3 = xx3} =
  g
    { bird1 = nextHead g <| s,
      x1 = (xx1 - 1) `mod` width,
      x2 = (xx2 - 1) `mod` width,
      x3 = (xx3 - 1) `mod` width,
      gameState = if isdie g then 4 else 1,
      self_win = if isdie g then 1 else 0
    }
move _ = error "Snakes can't be empty!"

-- | Get next head position of the snake
-- nextHead :: Game -> Coord
nextHead PS {dir = d, bird1 = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
nextHead _ = error "Snakes can't be empty!"

-- moveHead :: Game -> Coord
moveHead PS {dir = d, bird1 = (a :<| _)} = a & _y %~ (\y -> y + 4)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
-- turn :: Direction -> Game -> Game
turn d g@PS {bird1 = (s :|> _)} =
  g
    { bird1 = (moveHead g <| s)
    }

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

-- File IO
addscorelist :: PlayState -> [Int] -> PlayState
addscorelist
  g@PS
    { historyscore = _
    }
  h = g {historyscore = h}

--
writescore :: PlayState -> IO PlayState
writescore g@PS {score = s} =
  do
    let x = show s
    _ <- appendFile filename (x ++ "\n")
    return g

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
      -- gs <- net_lookup "gameState"
      -- print gs

      net_insert "gameState" 1
      -- gs <- net_lookup "gameState"
      -- print gs 
      let m = splitOn "," (C.unpack msg)
      -- print $ show (C.unpack msg)
      case C.unpack msg of
        "exit" -> do
          -- print "TCP server socket is closing now."
          close conn
          -- close sock
          return ()
        _ -> do
          -- print "TCP server socket is closing now."
          let n = length m
          net_insert "bird_comp" (read (m !! 0))
          net_insert "comp_win" (read (m !! 1))
          x <- net_lookup "bird_self"
          pl1 <- net_lookup "pl1"
          pl2 <- net_lookup "pl2"
          pl3 <- net_lookup "pl3"
          x1 <- net_lookup "x1"
          x2 <- net_lookup "x2"
          x3 <- net_lookup "x3"
          self_win <- net_lookup "self_win"
          
          let msg = C.pack (show x ++ "," ++ show pl1 ++ "," ++ show pl2 ++ "," ++ show pl3 ++ "," ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show self_win)
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
  return ()
  where
    rrLoop sock = do
      x <- net_lookup "bird_self"
      -- score <- net_lookup "score"
      net_insert "bird_comp" x
      self_win <- net_lookup "self_win"
      let send_msg = show x ++ "," ++ show self_win
      sendAll sock $ C.pack send_msg

      msg <- recv sock 1024
      -- print ("TCP client received: " ++ C.unpack msg)
      case (C.unpack msg) of
        "exit" -> do
          -- print "TCP server socket is closing now."
          close sock
          -- close sock
          return ()
        _ -> do
          -- print "hhhh"
          let m = splitOn "," (C.unpack msg)
          -- print $ m
          net_insert "bird_comp" (read (m !! 0))
          net_insert "pl1" (read (m !! 1))
          net_insert "pl2" (read (m !! 2))
          net_insert "pl3" (read (m !! 3))
          net_insert "x1" (read (m !! 4))
          net_insert "x2" (read (m !! 5))
          net_insert "x3" (read (m !! 6))
          net_insert "comp_win" (read (m !! 7))

          -- print ("TCP client received: " ++ C.unpack msg)
          threadDelay 100000

          rrLoop sock
