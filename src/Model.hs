{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Score  as Score
import qualified Model.Player as Player

import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Linear.V2 (V2 (..), _x, _y)
import System.IO
import Brick hiding (Direction)
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A

import Data.Sequence hiding (fromList)
import qualified Data.Sequence as S
import System.Random (Random (..), getStdRandom, newStdGen)
-- import View (height, width, gapSize, offset)

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 

data St =
    St { _focusRing :: F.FocusRing String
       , _edit1 :: E.Editor String String
       , _edit2 :: E.Editor String String
       }

type Coord = V2 Int

data Stream a = a :| Stream a
  deriving (Show)

height, width, gapSize, offset :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6
birdXPos = width `div` 4

-- gameOverAttr, snakeAttr, birdAttr, emptyAttr, bonusAttr :: AttrName
gameOverAttr = attrName "gameOver"
snakeAttr = attrName "snakeAttr"
birdAttr = attrName "birdAttr"
emptyAttr = attrName "emptyAttr"
bonusAttr = attrName "bonusAttr"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    , (E.editAttr, V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (snakeAttr, V.blue `on` V.blue)
    , (birdAttr, V.red `on` V.red)
    , (bonusAttr, V.yellow `on` V.yellow)
    , (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data PlayState = PS
  { gameState :: Int
  , isNetwork :: Int
  , isServer :: Int
  , choices :: D.Dialog Int
  , choices2 :: D.Dialog Int
  , st :: St
  , st2msg :: String
  , birdY :: Int
  , historyscore :: [Int]
  , bonus :: Coord
  , pl1 :: Int
  , pl2 :: Int
  , pl3 :: Int
  , x1 :: Int
  , x2 :: Int
  , x3 :: Int
  , wall :: Int
  , randP :: Int
  , randPs :: Stream Int
  , bird1 :: Seq Coord
  , bird2 :: Seq Coord
  , score :: Int
  , bonusList :: Stream Coord
  , dir :: Direction
  , nextBonus :: Coord
  }

-- init :: Int -> PlayState
-- init n = PS 
--   { gameState = 0
--   , isNetwork = 0
--   , isServer = 0
--   , choices = initChoices
--   , choices2 = initChoices2
--   , st = St (F.focusRing ["Edit1", "Edit2"])
--        (E.editor "Edit1" Nothing "")
--        (E.editor "Edit2" (Just 2) "")
--   , st2msg = "Press Enter to confirm, Esc to quit."
--   , birdY = 0
--   }

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")


-- initGame :: IO Game
initGame = do
  -- contents <- readFile "/home/cse230/Desktop/test.txt"
  (bo :| bs) <-
    fromList . randomRs (V2 birdXPos (height `div` 4), V2 birdXPos (height * 3 `div` 4)) <$> newStdGen
  -- streaming of random pillar length
  (randp :| randps) <-
    fromList . randomRs (0 + offset, (height `div` 3) + offset) <$> newStdGen
  -- hard code initial pillar length
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)
  let xm = birdXPos
      ym = height `div` 2
      bonusx = 15
      bonusy = 15
      -- x = init $ split contents
      -- y = sort [ read a::Integer | a <-x]
      -- result = take 5 y
      g =
        PS
          { bird1 = S.singleton (V2 xm ym),
            bird2 = S.singleton (V2 xm ym),
            bonus = V2 birdXPos bonusy,
            bonusList = bs,
            score = 0,
            dir = South,
            historyscore = [], -- result TODO
            -- from C branch
            randP = randp,
            randPs = randps,
            pl1 = a,
            pl2 = b,
            pl3 = c,
            x1 = width - 1,
            x2 = width * 2 `div` 3,
            x3 = width `div` 3,
            wall = 0,
            gameState = 0,
            isNetwork = 0, 
            isServer = 0,
            choices = initChoices,
            choices2 = initChoices2,
            st = St (F.focusRing ["Edit1", "Edit2"])
                (E.editor "Edit1" Nothing "")
                (E.editor "Edit2" (Just 2) ""),
            st2msg = "Press Enter to confirm, Esc to quit.",
            birdY = 0,
            nextBonus = bo
          }
  return g

initChoices :: D.Dialog Int
initChoices = D.dialog (Just "Choose Game Mode") (Just (0, choices)) 50
    where
        choices = [ ("Single", 0)
                  , ("Double", 1)
                  ]

initChoices2 :: D.Dialog Int
initChoices2 = D.dialog (Just "Double Model Chosen") (Just (0, choices)) 50
    where
        choices = [ ("Start Server", 0)
                  , ("Join Server", 1)
                  ]

-- isCurr :: PlayState -> Int -> Int -> Bool
-- isCurr s r c = Board.pRow p == r && Board.pCol p == c
--   where 
--     p = psPos s

-- next :: PlayState -> Board.Result Board.Board -> Either (Board.Result ()) PlayState
-- next s Board.Retry     = Right s
-- next s (Board.Cont b') = Right (s { psBoard = b'
--                                   , psTurn  = Board.flipXO (psTurn s) })
-- next s res             = nextBoard s res 

-- nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
-- nextBoard s res = case res' of
--                     Board.Win _ -> Left res' 
--                     Board.Draw  -> Left res'
--                     _           -> Right s' 
--   where 
--     sc'  = Score.add (psScore s) (Board.boardWinner res) 
--     res' = Score.winner sc'
--     s'   = s { psScore = sc'                   -- update the score
--              , psBoard = mempty                -- clear the board
--              , psTurn  = Score.startPlayer sc' -- toggle start player
--              } 

