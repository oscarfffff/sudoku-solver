{-# LANGUAGE DataKinds #-}

module Generate where

import Control.Monad (replicateM)
import Data.List
import Data.List.Split (chunksOf)
import Language.Hasmtlib
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

buildRandomCell :: IO Cell
buildRandomCell = do
  randomValue <- randomRIO (0 :: Int, 9)
  randomRow <- randomRIO (0 :: Int, 8)
  randomColumn <- randomRIO (0 :: Int, 8)
  return (randomValue, (randomRow, randomColumn))

printMatrix arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 9 arr

checkIfUniqueSolution :: [Cell] -> IO Bool
checkIfUniqueSolution preDefValues = do
  res <- solveSudoku preDefValues []
  case res of
    (Sat, Just solution) -> do
      res2 <- solveSudoku preDefValues $ encode solution
      case res2 of
        (Sat, Just solution2) -> do
          print "Found solution but not unique"
          printMatrix $ concat solution2
          return False
        _ -> do
          putStrLn "Found unique solution"
          return True
    _ -> do
      putStrLn "Not a valid solution"
      return False

createCompleteBoard numberOfRandomCells = do
  randomCells <- replicateM (fromIntegral numberOfRandomCells) buildRandomCell
  res <- solveSudoku randomCells []
  case res of
    (Sat, Just board) -> return board
    _ -> createCompleteBoard numberOfRandomCells

createRandomBoardUpwards :: Integer -> [Cell] -> IO [Cell]
createRandomBoardUpwards numberOfNewCells currentCells = do
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
        False -> createRandomBoardUpwards 5 randomCells
    _ -> do
      print "Solution was not valid"
      createRandomBoardUpwards 5 currentCells

reduceBoard :: [Cell] -> Integer -> IO [Cell]
reduceBoard preDefValues allowedFailures = do
  print "Number of allowed Failures: "
  print allowedFailures
  reducedBoard <- deleteRandomElements 3 preDefValues
  if allowedFailures == 0
    then return preDefValues
    else do
      res <- checkIfUniqueSolution $ encode reducedBoard
      print res
      case res of
        False -> reduceBoard preDefValues (allowedFailures - 1)
        True -> reduceBoard reducedBoard 15

convertSolutionToIndexedArray :: [a] -> [(a, (Int, Int))]
convertSolutionToIndexedArray arr = zipWith (\i x -> (x, (i `mod` 9, i `div` 9))) [0 ..] arr

deleteRandomElements :: Int -> [a] -> IO [a]
deleteRandomElements n xs = do
  indices <- replicateM n (randomRIO (0, length xs - 1))
  let xs' = removeAtIndices indices xs
  return xs'

removeAtIndices :: [Int] -> [a] -> [a]
removeAtIndices indices xs = foldr (\i acc -> take i acc ++ drop (i + 1) acc) xs (reverse indices)
