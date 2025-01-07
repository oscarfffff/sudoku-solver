{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (replicateM)
import Data.List
import Language.Hasmtlib
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

buildRandomCell :: IO Cell
buildRandomCell = do
  randomValue <- randomRIO (0 :: Int, 9)
  randomRow <- randomRIO (0 :: Int, 8)
  randomColumn <- randomRIO (0 :: Int, 8)
  return (randomValue, (randomRow, randomColumn))

checkIfUniqueSolution :: [Cell] -> IO Bool
checkIfUniqueSolution preDefValues = do
  res <- solveSudoku preDefValues []
  case res of
    (Sat, Just solution) -> do
      res2 <- solveSudoku preDefValues $ encode solution
      case res2 of
        (Sat, _) -> return False
        _ -> return True
    _ -> return False

createRandomBoard :: IO [Cell]
createRandomBoard = do
  randomCells <- replicateM 10 buildRandomCell
  res <- solveSudoku randomCells []
  case res of
    (Sat, _) -> return randomCells
    _ -> createRandomBoard

main :: IO ()
main = do
  let setNumbers = [(9, (0, 2)), (8, (0, 3)), (5, (0, 5)), (4, (0, 7)), (7, (0, 8))]
  res <- checkIfUniqueSolution setNumbers
  case res of
    True -> print "Is unique"
    False -> print "Is not unique"
