{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import Snake

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 11 $
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawGameOver (g ^. dead)
      ]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $
      C.hCenter $
        padAll 1 $
          str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
    then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
    else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Snake") $
      hBox columns
  where
    columns = [vBox $ cellsInCol c | c <- [0 .. width -1]]
    cellsInCol x = [drawCoord (V2 x y) | y <- [height -1, height -2 .. 0]]
    drawCoord = drawCell . cellAt
    cellAt c
      | isPillar g c = Snake
      | otherwise = Empty

-- | c `elem` g ^. snake = Snake
-- pillarlength [0,pl] U [pl+gapSize, height]
-- x: x1, x2, x3 &&
isPillar :: Game -> (V2 Int) -> Bool
isPillar g (V2 x y)
  | x == g ^. x1 && (y `elem` [0 .. g ^. pl1] ++ [g ^. pl1 + gapSize .. height]) = True
  | x == g ^. x2 && (y `elem` [0 .. g ^. pl2] ++ [g ^. pl2 + gapSize .. height]) = True
  | x == g ^. x3 && (y `elem` [0 .. g ^. pl3] ++ [g ^. pl3 + gapSize .. height]) = True
  | otherwise = False

gapSize :: Int
gapSize = height * 3 `div` 10

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (snakeAttr, V.blue `on` V.blue),
      (foodAttr, V.red `on` V.red),
      (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
