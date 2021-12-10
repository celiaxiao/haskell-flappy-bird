module Main where

import Brick
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import Brick.Util (bg, on)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import Control
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Model
import System.Environment (getArgs)
import Text.Read (readMaybe)
import View

-------------------------------------------------------------------------------

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 200000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  g <- initGame
  res <- customMain initialVty buildVty (Just chan) app g
  return ()

app :: App PlayState Tick String
app =
  App
    { appDraw = view,
      appChooseCursor = const . const Nothing,
      appHandleEvent = control,
      appStartEvent = return,
      appAttrMap = const theMap
    }
