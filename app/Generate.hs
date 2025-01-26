{-# LANGUAGE DataKinds #-}

module Generate where

import Control.Monad (replicateM)
import Language.Hasmtlib hiding ((||))
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

buildRandomCell :: IO Cell
buildRandomCell = do
  randomValue <- randomRIO (0 :: Int, 9)
  randomRow <- randomRIO (0 :: Int, 8)
  randomColumn <- randomRIO (0 :: Int, 8)
  return (randomValue, (randomRow, randomColumn))

-- determines if the board with given fields has a unique solution
checkIfUniqueSolution :: [Cell] -> IO Bool
checkIfUniqueSolution preDefValues = do
  res <- solveSudoku preDefValues []
  case res of
    (Sat, Just solution) -> do
      res2 <- solveSudoku preDefValues $ encode solution
      case res2 of
        (Sat, Just solution2) -> do
          return False
        _ -> do
          return True
    _ -> do
      return False

-- creates a solved board with n initial random values
createCompleteBoard :: Int -> IO [[Integer]]
createCompleteBoard numberOfRandomCells = do
  randomCells <- replicateM (fromIntegral numberOfRandomCells) buildRandomCell
  res <- solveSudoku randomCells []
  case res of
    (Sat, Just board) -> return board
    _ -> createCompleteBoard numberOfRandomCells

-- takes a complete solved board and deletes elements until the solution becomes non unique
reduceBoard :: [Cell] -> Integer -> IO [Cell]
reduceBoard preDefValues allowedFailures = do
  putStrLn "------------------------------------------------------------------"
  putStrLn "Number of allowed Failures left: "
  print allowedFailures
  putStrLn "Number of prefilled Cells: "
  print $ length preDefValues
  reducedBoard <- deleteRandomElements 1 preDefValues
  if allowedFailures == 0
    then return preDefValues
    else do
      res <- checkIfUniqueSolution $ encode reducedBoard
      case res of
        False -> reduceBoard preDefValues (allowedFailures - 1)
        True -> reduceBoard reducedBoard 20

-- takes a falttened solution and convertes it into an array of cells
convertSolutionToIndexedArray :: [Int] -> [Cell]
convertSolutionToIndexedArray arr = zipWith (\i x -> (x, (i `mod` 9, i `div` 9))) [0 .. length arr - 1] arr

deleteRandomElements :: Int -> [a] -> IO [a]
deleteRandomElements n xs = do
  indicesToDelete <- replicateM n (randomRIO (0, length xs - 1))
  return $ removeAtIndices indicesToDelete xs

removeAtIndices :: [Int] -> [a] -> [a]
removeAtIndices indices xs =
  foldr (\i acc -> take i acc ++ drop (i + 1) acc) xs (reverse indices)
