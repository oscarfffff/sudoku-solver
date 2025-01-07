{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (replicateM)
import Generate
import Language.Hasmtlib
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

solveUpwards = do
  board <- createRandomBoardUpwards 10 []
  print board

solveDownwards = do
  board <- createCompleteBoard 10
  reducedBoard <- reduceBoard $ convertSolutionToIndexedArray $ concatMap (map fromIntegral) board
  print reducedBoard
  print $ length reducedBoard

main :: IO ()
main = do
  solveDownwards
