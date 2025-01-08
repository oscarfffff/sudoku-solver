{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Generate
import Language.Hasmtlib
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

solveUpwards = do
  board <- createRandomBoardUpwards 10 []
  print board

solveDownwards = do
  board <- createCompleteBoard 10
  reducedBoard <- reduceBoard (convertSolutionToIndexedArray $ concatMap (map fromIntegral) board) 15
  return reducedBoard

-- Create an empty 9x9 grid filled with 'x'
emptyGrid :: [[Char]]
emptyGrid = replicate 9 (replicate 9 '*')

-- Update the grid with the given values
updateGrid :: [[Char]] -> [(Int, (Int, Int))] -> [[Char]]
updateGrid grid updates = foldl updateCell grid updates
  where
    updateCell g (val, (row, col)) =
      let updatedRow = take col (g !! row) ++ show val ++ drop (col + 1) (g !! row)
       in take row g ++ [updatedRow] ++ drop (row + 1) g

addSpaces :: [[Char]] -> [[Char]]
addSpaces = map (intercalate " " . map (: []))

-- Print the grid
printGrid :: [[Char]] -> IO ()
printGrid grid = putStrLn $ intercalate "\n" (addSpaces grid)

main :: IO ()
main = do
  board <- solveDownwards
  print "------------------------------------------------------------------"
  printGrid $ updateGrid emptyGrid board
  print $ length board
  print "------------------------------------------------------------------"
