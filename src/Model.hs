{-# LANGUAGE RecordWildCards #-}

module Model where

import Brick hiding (Direction)
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import Data.List (sort, sortBy, take)
import Data.Sequence hiding (fromList)
import qualified Data.Sequence as S hiding (sortBy)
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))
import System.IO
import System.Random (Random (..), getStdRandom, randomRIO)
import Prelude hiding ((!!))

-------------------------------------------------------------------------------

-- | Ticks mark passing of time: a custom event that we constantly stream

-------------------------------------------------------------------------------
data Tick = Tick

data State
  = Intro
  | Play PlayState
  | Outro

data St = St
  { _focusRing :: F.FocusRing String,
    _edit1 :: E.Editor String String,
    _edit2 :: E.Editor String String
  }

type Coord = V2 Int

data Stream a = a :| Stream a
  deriving (Show)

height, width, gapSize, offset, birdXPos, pListLen, bListLen :: Int
height = 30
width = 30
gapSize = height * 3 `div` 10
offset = height `div` 6
birdXPos = width `div` 4
pListLen = 100
bListLen = 30

filename :: String
filename = "leaderboard.txt"

-- gameOverAttr, snakeAttr, birdAttr, emptyAttr, bonusAttr :: AttrName
gameOverAttr = attrName "gameOver"

snakeAttr = attrName "snakeAttr"

birdAttr = attrName "birdAttr"

emptyAttr = attrName "emptyAttr"

bonusAttr = attrName "bonusAttr"

bird2Attr = attrName "bird2Attr"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow),
      (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (snakeAttr, V.blue `on` V.blue),
      (birdAttr, V.red `on` V.red),
      (bonusAttr, V.yellow `on` V.yellow),
      (bird2Attr, V.green `on` V.green),
      (gameOverAttr, fg V.red `V.withStyle` V.bold)
    ]

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

data PlayState = PS
  { gameState :: Int,
    isNetwork :: Int,
    isServer :: Int,
    choices :: D.Dialog Int, -- single player
    choices2 :: D.Dialog Int, -- multi player 
    st :: St,
    st2msg :: String,
    birdY :: Int,
    historyscore :: [Int],
    bonus :: Coord,
    pl1 :: Int,
    pl2 :: Int,
    pl3 :: Int,
    x1 :: Int,
    x2 :: Int,
    x3 :: Int,
    randP :: Int,
    randPs :: [Int],
    bird1 :: Seq Coord,
    bird2 :: Seq Coord,
    score :: Int,
    bonusList :: [Int],
    dir :: Direction,
    nextBonus :: Int,
    self_win :: Int,
    comp_win :: Int,
    win :: Int
  }


split :: String -> [String]
split [] = [""]
split (c : cs)
  | c == '\n' = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x, y))

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

randomList :: Int -> Int -> Int -> IO [Int]
randomList 0 _ _ = return []
randomList n u l = do
  r <- randomRIO (u, l)
  rs <- randomList (n -1) u l
  return (r : rs)

sortDes :: [Int] -> [Int]
sortDes = Data.List.sortBy (flip compare)


--
initGame = do
  a <- drawInt (0 + offset) ((height `div` 3) + offset)
  b <- drawInt (0 + offset) ((height `div` 3) + offset)
  c <- drawInt (0 + offset) ((height `div` 3) + offset)
  bonusy <- drawInt (0 + offset) ((height `div` 3) + offset)
  pillarLenList <- randomList pListLen offset ((height `div` 3) + offset)
  bonusPosList <- randomList bListLen (height `div` 4) (height * 3 `div` 4)
  contents <- readFile filename
  let xm = birdXPos
      ym = height `div` 2
      x = init $ split contents
      result = Data.List.sort [read a :: Int | a <- x]
      g =
        PS
          { bird1 = S.singleton (V2 xm ym),
            bird2 = S.singleton (V2 xm ym),
            bonus = V2 birdXPos bonusy,
            bonusList = bonusPosList,
            score = 0,
            dir = South,
            historyscore = result,
            -- from C branch
            randP = 0,
            randPs = pillarLenList,
            pl1 = a,
            pl2 = b,
            pl3 = c,
            x1 = width - 1,
            x2 = width * 2 `div` 3,
            x3 = width `div` 3,
            gameState = 0,
            isNetwork = 0,
            isServer = 0,
            choices = initChoices,
            choices2 = initChoices2,
            st =
              St
                (F.focusRing ["Edit1", "Edit2"])
                (E.editor "Edit1" Nothing "")
                (E.editor "Edit2" (Just 2) ""),
            st2msg = "Press Enter to confirm, Esc to quit.",
            birdY = 0,
            nextBonus = 0,
            self_win = 0,
            comp_win = 0,
            win = 0
          }
  return g

initChoices :: D.Dialog Int
initChoices = D.dialog (Just "Choose Game Mode") (Just (0, choices)) 50
  where
    choices =
      [ ("Single", 0),
        ("Double", 1)
      ]

initChoices2 :: D.Dialog Int
initChoices2 = D.dialog (Just "Double Model Chosen") (Just (0, choices)) 50
  where
    choices =
      [ ("Start Server", 0),
        ("Join Server", 1)
      ]
