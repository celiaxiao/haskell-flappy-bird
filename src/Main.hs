module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Model
import View
import Control 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import qualified Brick.Widgets.Dialog as D
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Edit as E
import Brick.Util (on, bg)
-------------------------------------------------------------------------------

-- makeLenses ''St

main :: IO ()
main = do
  rounds <- fromMaybe defaultRounds <$> getRounds
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  g <- initGame
  res <- customMain initialVty buildVty (Just chan) app g --(Model.init rounds)
  return ()

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

getRounds :: IO (Maybe Int)
getRounds = do
  args <- getArgs
  case args of
    (str:_) -> return (readMaybe str)
    _       -> return Nothing

defaultRounds :: Int
defaultRounds = 3
