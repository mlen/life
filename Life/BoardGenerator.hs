module Life.BoardGenerator where

import System.Random

import Life.Board
import Life.Game

randomBoard :: Int -> Int -> IO Cell
randomBoard _ _ = do
    r <- randomRIO (0, 3) :: IO Int
    if r `mod` 3 == 0
      then return Alive
      else return Dead
