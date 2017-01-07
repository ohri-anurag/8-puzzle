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
      solution = astar
        (fromList list (fromJust $ elemIndex 0 list))
        (fromList [1,2,3,4,5,6,7,8,0] 8)
        neighbors
        manhattan
        uid
    mapM_ (\(i,s) -> putStrLn ("-- " ++ show i ++ " --") >> putStrLn s) $ zip [1..] $ map show solution
    putStrLn ("Steps Taken : " ++ show (length solution - 1))
