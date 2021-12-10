import Test.QuickCheck
import Data.List (sort, sortBy)

-- prop_revapp :: [Int] -> [Int] -> Bool
-- prop_revapp xs ys =
--   reverse (xs ++ ys) == reverse xs ++ reverse ys


---------------------------------------------------------
    
testSortDes :: [Int] -> Bool
testSortDes xs = reverse (sort xs) == sortDes xs
  where 
    types = xs :: [Int]

-- >>> quickCheck testSortDes
-- +++ OK, passed 100 tests.
--


testNextPZero :: Int -> Int -> Int -> Property
testNextPZero x rp p = 
  (x == 0) ==> (nextP x rp p == rp)

testNextPNonZero :: Int -> Int -> Int -> Property
testNextPNonZero x rp p = 
  (x /= 0) ==> (nextP x rp p == p)


-- >>> quickCheck testNextPNonZero
-- +++ OK, passed 100 tests; 13 discarded.

-- >>> quickCheck testNextPZero
-- *** Gave up! Passed only 26 tests; 1000 discarded tests.
--



testCollideX :: Int -> Int -> Int -> Int -> Property
testCollideX bx by px py = 
  (bx /= px) ==> (collide bx by px py == False)

testCollideY :: Int -> Int -> Int -> Int -> Property
testCollideY bx by px py = 
  (by `elem` [py+1 .. py+gapSize-1]) ==> (collide bx by px py == False)

-- >>> quickCheck testCollideX
-- +++ OK, passed 100 tests; 12 discarded.
--

-- >>> quickCheck testCollideY
-- +++ OK, passed 100 tests; 659 discarded.
--

---------------------------------------------------------------
-- Functions from src to test
---------------------------------------------------------------

sortDes :: [Int] -> [Int]
sortDes = sortBy (flip compare)

nextP :: Int -> Int -> Int -> Int
nextP x rp p = if x /= 0 then p else rp


gapSize, height, offset :: Int
height = 50
gapSize = height * 3 `div` 10
offset = height `div` 6

collide :: Int -> Int -> Int -> Int -> Bool
collide bx by px py = 
  bx == px && (by `elem` [0 .. py] ++ [py + gapSize .. height]) 
