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
        (Sat, _) -> do
          print "Found solution but not unique"
          print res2
          return False
        _ -> do
          putStrLn "Found unique solution"
          return True
    _ -> do
      putStrLn "Not a valid solution"
      return False

createRandomBoard :: Integer -> [Cell] -> IO [Cell]
createRandomBoard numberOfNewCells currentCells = do
  newCells <- replicateM (fromIntegral numberOfNewCells) buildRandomCell
  let randomCells = currentCells ++ newCells
  print "Current number of cells:"
  print $ length randomCells
  res <- solveSudoku randomCells []
  case res of
    (Sat, _) -> do
      putStrLn "Found solution"
      sol <- checkIfUniqueSolution randomCells
      case sol of
        True -> return randomCells
        False -> createRandomBoard 5 randomCells
    _ -> do
      print "Solution was not valid"
      createRandomBoard 5 currentCells

main :: IO ()
main = do
  board <- createRandomBoard 10 []
  print board
  print $ length board
