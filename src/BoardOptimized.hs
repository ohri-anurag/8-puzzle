module BoardOptimized(
  fromList,
  neighbors,
  manhattan,
  uid,
  isSolvable
)where

import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Vector as V

data Board =
  Board { vector :: V.Vector Int -- ^ A vector that stores the 8-puzzle
        , uid :: Int64           -- ^ An integer that uniquely represents the current 8-puzzle
        , manhattan :: Int       -- ^ The value of manhattan heuristic for this puzzle.
        , zp :: Int              -- ^ The position of zero in the 8-puzzle (its index in the vector)
        }

instance Eq Board where
  x == y = uid x == uid y

instance Show Board where
  show = display

-- | Creates a Board from a list of integers and the index of zero in that list
fromList :: [Int] -> Int -> Board
fromList b zp = let b' = V.fromList b in Board b' (uidLocal b') (manhattanLocal b') zp

-- Following functions create a new board by moving the zero up, down, left and right.
up board = move (\z -> z >= 3) (\z -> z - 3) board

down board = move (\z -> z < 6) (\z -> z + 3) board

left board = move (\z -> mod z 3 > 0) (\z -> z - 1) board

right board = move (\z -> mod z 3 < 2) (\z -> z + 1) board

move :: (Int -> Bool) -> (Int -> Int) -> Board -> Maybe Board
move pred op (Board board uid mnh zp)
  | pred zp =
    let
      board' = (board V.// [(zp, board V.! zp'), (zp', board V.! zp)])
    in
      Just $ Board board' (uidLocal board') (manhattanLocal board') zp'
                           
  | otherwise = Nothing
  where
    zp' = op zp

-- | Generates the neighbors of the current board, and returns them as a list.
neighbors :: Board -> [Board]
neighbors brd@(Board b uid mnh zp) =
  map fromJust $ filter isJust $ map (\f -> f brd) [up, down, left, right]

diff :: Int -> Int -> Int
diff new old = abs (nx - ox) + abs (ny - oy)
  where
    nx = div new 3
    ny = mod new 3
    ox = div new 3
    oy = mod old 3

-- | Calculate the manhattan heuristic for a vector.
-- This is done as follows:
-- For each integer in the puzzle except for zero, we calculate its current x and y coords. We also calculate its proper x and y coords (ie the location
-- where it should be in the final 8-puzzle). Then we calculate the sum of deltas of both coords.
manhattanLocal :: V.Vector Int -> Int
manhattanLocal v = V.foldr (\(index, num) acc ->
                     if num /= 0
                       then acc + diff index (num - 1)
                       else acc
                   ) 0 (V.zip (V.fromList [0..8]) v)

-- | Checks if a puzzle is solvable.
-- For this purpose, we calculate the inversion count of the vector, excluding the integer zero.
isSolvable :: Board -> Bool
isSolvable (Board board _ _ _) = 0 == mod (sum $ map f $ zip b' $ tail (inits b')) 2
  where
    b' = V.toList board
    f (n, xs)
      | n == 0    = 0
      | otherwise = length $ filter (\x -> x /= 0 && x > n) xs

-- | Generate an unique identifier by concatenating the integers in the puzzle.
uidLocal :: V.Vector Int -> Int64
uidLocal v = V.foldl' (\acc x -> acc*10 + fromIntegral(x)) 0 v


display :: Board -> String
display (Board b _ _ _) = helper (V.toList b)
  where
    helper [] = ""
    helper xs = (intersperse ' ' $ concatMap show $ take 3 xs) ++ "\n" ++ helper (drop 3 xs)
