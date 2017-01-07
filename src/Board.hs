module Board(
  fromList,
  neighbors,
  manhattan,
  uid,
  display
)where

import Data.List
import Data.Maybe
import qualified Data.Vector as V

type Board = V.Vector Int

fromList :: [Int] -> Board
fromList = V.fromList

zeroPos :: Board -> Int
zeroPos = fromJust . V.elemIndex 0

up sz board = move (\z -> z >= sz) (\z -> z - sz) board

down sz board = move (\z -> z < sz * (sz - 1)) (\z -> z + sz) board

left sz board = move (\z -> mod z sz > 0) (\z -> z - 1) board

right sz board = move (\z -> mod z sz < (sz - 1)) (\z -> z + 1) board

move :: (Int -> Bool) -> (Int -> Int) -> Board -> Maybe Board
move pred op board
  | pred zp = Just $ board V.// [(zp, board V.! zp'), (zp', board V.! zp)]
  | otherwise = Nothing
  where
    zp = zeroPos board
    zp' = op zp

neighbors :: Board -> [Board]
neighbors board = map fromJust $ filter isJust $ map (\f -> f sz board) [up, down, left, right]
  where
    sz = floor . sqrt . fromIntegral $ (V.length board)

diff :: Int -> Int -> Int -> Int
diff new old sz = abs (ni - oi) + abs (nj - oj)
  where
    ni = div new sz
    nj = mod new sz
    oi = div new sz
    oj = mod old sz

manhattan :: Board -> Int
manhattan board = foldr (\(index, num) acc ->
                         if num /= 0
                           then acc + diff index (num - 1) sz
                           else acc
                      ) 0 (zip [0..(sz*sz - 1)] (V.toList board))
  where
    sz = floor . sqrt . fromIntegral $ (V.length board)

isSolvable :: Board -> Bool
isSolvable board = 0 == mod (sum $ map f $ zip b' $ tail (inits b')) 2
  where
    b' = V.toList board
    f (n, xs)
      | n == 0    = 0
      | otherwise = length $ filter (\x -> x > n) xs

uid :: Board -> Integer
uid b = V.foldr (\x acc -> acc*10 + fromIntegral(x)) 0 b

display :: Board -> String
display b = helper (V.toList b)
  where
    sz = floor . sqrt . fromIntegral $ (V.length b)
    helper [] = ""
    helper xs = (intersperse ' ' $ concatMap show $ take sz xs) ++ "\n" ++ helper (drop sz xs)
