module View (view) where

import Brick
import qualified Brick.Focus as F
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import Data.List
import Linear.V2 (V2 (..))
import Model

data Cell = Snake | Bird | Bonus | Empty | BirdTwo

view :: PlayState -> [Widget String]
view s = case gameState s of
  0 -> view0 s
  1 -> view1 s
  2 -> view2 s
  3 -> view3 s
  4 -> view4 s

-- display start 
view0 :: PlayState -> [Widget n]
view0 s = [ui]
  where
    ui = D.renderDialog (choices s) $ C.hCenter $ padAll 1 $ str "   "
    -- choices: single/double

-- display game
view1 :: PlayState -> [Widget n]
view1 g@PS {isNetwork = s} = case s of
  0 -> [C.center $ padRight (Pad 2) (drawStats g) <+> drawGridSingle g]
  1 -> [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

-- display start/ join server 
view2 :: PlayState -> [Widget n]
view2 s = [ui]
  where
    ui = D.renderDialog (choices2 s) $ C.hCenter $ padAll 1 $ str "   "
    -- choices2: start/join server

-- display join server 
view3 :: PlayState -> [Widget String]
view3 s = [ui]
  where
    t = (st s)
    e1 = F.withFocusRing (_focusRing t) (E.renderEditor (str . unlines)) (_edit1 t)
    e2 = F.withFocusRing (_focusRing t) (E.renderEditor (str . unlines)) (_edit2 t)

    ui =
      C.center $
        (str "IP: " <+> (hLimit 20 $ vLimit 1 e1))
          <=> str " "
          <=> (str "Port: " <+> (hLimit 5 $ vLimit 1 e2))
          <=> str " "
          <=> str (st2msg s)

-- display game over 
view4 :: PlayState -> [Widget n]
view4 s = [C.center $ padRight (Pad 2) (drawGameOver s)]

drawScore :: Show a => a -> Widget n
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $
      C.hCenter $
        padAll 1 $
          str $ show n

drawStats :: PlayState -> Widget n
drawStats g@PS {gameState = d} = case d of
  1 ->
    hLimit 11 $
      vBox
        [ drawScore (score g),
          padTop (Pad 2) emptyWidget
        ]

drawGameOver :: PlayState -> Widget n
drawGameOver PS {historyscore = history, score = s, self_win = sw, comp_win = cw, isNetwork=inn} =
  vBox $
    (winmsg) :
    str "   Game Over" :
    str (" Your Score is: " ++ (show s)) :
    str "To save the score in leaderboard, press s" :
    str "   LeaderBoard:" :
    (str <$> ["      " <> show i | i <- take 5 $ reverse $ sort (history)])
    where
      winmsg = case (inn, sw, cw) of
        (1, 1, 0) -> str "You lose!"
        (1, 0, 1) -> str "You win!"
        _ -> str ""

drawGrid :: PlayState -> Widget n
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Flappy Bird") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` bird2 g = BirdTwo
      | c `elem` bird1 g = Bird
      | isPillar g c = Snake
      | otherwise = Empty

drawGridSingle :: PlayState -> Widget n
drawGridSingle g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Flappy Bird") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` bird1 g = Bird
      | isPillar g c = Snake
      | isBonus g c = Bonus
      | otherwise = Empty

isPillar :: PlayState -> V2 Int -> Bool
isPillar g (V2 x y)
  | x == (x1 g) && (y `elem` [0 .. (pl1 g)] ++ [(pl1 g) + gapSize .. height]) = True
  | x == (x2 g) && (y `elem` [0 .. (pl2 g)] ++ [(pl2 g) + gapSize .. height]) = True
  | x == (x3 g) && (y `elem` [0 .. (pl3 g)] ++ [(pl3 g) + gapSize .. height]) = True
  | otherwise = False

isBonus :: PlayState -> V2 Int -> Bool
isBonus PS {bonus = (V2 xb yb)} (V2 x y) = xb == x && yb == y

drawCell :: Cell -> Widget n
drawCell Snake = withAttr snakeAttr cw
drawCell Bird = withAttr birdAttr cw
drawCell BirdTwo = withAttr bird2Attr cw
drawCell Bonus = withAttr bonusAttr cw
drawCell Empty = withAttr emptyAttr cw

cw = str "  "
