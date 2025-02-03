{-# LANGUAGE DataKinds #-}

module Generate where

import Control.Monad (replicateM)
import Language.Hasmtlib hiding ((||))
import Solver (Cell, solveSudoku)
import System.Random (randomRIO)

buildRandomCell :: IO Cell
buildRandomCell = do
  randomValue <- randomRIO (1 :: Int, 9)
  randomRow <- randomRIO (0 :: Int, 8)
  randomColumn <- randomRIO (0 :: Int, 8)
  return (randomValue, (randomColumn, randomRow))

-- determines if the board with given fields has a unique solution
checkIfUniqueSolution :: [Cell] -> IO Bool
checkIfUniqueSolution preDefValues = do
  res <- solveSudoku preDefValues Nothing
  case res of
    (Sat, Just solution) -> do
      res2 <- solveSudoku preDefValues $ Just $ encode solution
      case res2 of
        (Sat, _) -> do
          return False
        _ -> do
          return True
    _ -> do
      return False

-- creates a solved board with n initial random values
createCompleteBoard :: Int -> IO [[Integer]]
createCompleteBoard numberOfRandomCells = do
  randomCells <- replicateM (fromIntegral numberOfRandomCells) buildRandomCell
  res <- solveSudoku randomCells Nothing
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
  reducedBoard <- deleteRandomElement preDefValues
  if allowedFailures == 0
    then return preDefValues
    else do
      res <- checkIfUniqueSolution $ encode reducedBoard
      case res of
        False -> reduceBoard preDefValues (allowedFailures - 1)
        True -> reduceBoard reducedBoard 20

-- takes a falttened solutionarray and convertes it into an array of cells
convertSolutionToIndexedArray :: [Int] -> [Cell]
convertSolutionToIndexedArray arr = map (\i -> (arr !! i, (i `mod` 9, i `div` 9))) [0 .. length arr - 1]

deleteRandomElement :: [a] -> IO [a]
deleteRandomElement xs = do
  i <- randomRIO (0, length xs - 1)
  return $ take i xs ++ drop (i + 1) xs
