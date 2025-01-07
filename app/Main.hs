{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (replicateM)
import Generate
import Language.Hasmtlib
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

main :: IO ()
main = do
  board <- createCompleteBoard 10
  reducedBoard <- reduceBoard $ convertSolutionToIndexedArray $ concatMap (map fromIntegral) board
  print reducedBoard
  print $ length reducedBoard
