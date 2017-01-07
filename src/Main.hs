module Main where

import BoardOptimized
import AStar
import Data.Maybe(fromJust)
import Data.List(elemIndex)
import System.IO

main :: IO ()
main = do
  putStr $ "The goal puzzle is:\n" ++
    "1 2 3\n4 5 6\n7 8 0\nEnter the puzzle to be solved as a list: "
  hFlush stdout
  getLine >>= \input -> do
    let
      list = map (\x -> read (x : "") :: Int) (filter (' ' /= ) input)
      board = fromList list (fromJust $ elemIndex 0 list)
      solution = astar
        board
        (fromList [1,2,3,4,5,6,7,8,0] 8)
        neighbors
        manhattan
        uid
    if isSolvable board
      then
        do
          mapM_ (\(i,s) -> putStrLn ("-- " ++ show i ++ " --") >> putStrLn s) $ zip [1..] $ map show solution
          putStrLn ("Steps Taken : " ++ show (length solution - 1))
      else
        do
          putStrLn "This 8-puzzle cannot be solved. It cannot be converted to the goal state showed above. Try swapping two non-zero adjacent integers."          
