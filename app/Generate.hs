module Generate where

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

createCompleteBoard numberOfRandomCells = do
  randomCells <- replicateM (fromIntegral numberOfRandomCells) buildRandomCell
  res <- solveSudoku randomCells []
  case res of
    (Sat, Just board) -> return board
    _ -> createCompleteBoard numberOfRandomCells

reduceBoard :: [Cell] -> IO [Cell]
reduceBoard preDefValues = do
  -- Encode and check if the solution is unique
  res <- checkIfUniqueSolution $ encode preDefValues
  case res of
    False -> return preDefValues
    True -> do
      reducedBoard <- deleteRandomElements 1 preDefValues
      reduceBoard reducedBoard

convertSolutionToIndexedArray :: [a] -> [(a, (Int, Int))]
convertSolutionToIndexedArray arr = zipWith (\i x -> (x, (i `mod` 9, i `div` 9))) [0 ..] arr

deleteRandomElements :: Int -> [a] -> IO [a]
deleteRandomElements n xs = do
  indices <- replicateM n (randomRIO (0, length xs - 1))
  let xs' = removeAtIndices indices xs
  return xs'

removeAtIndices :: [Int] -> [a] -> [a]
removeAtIndices indices xs = foldr (\i acc -> take i acc ++ drop (i + 1) acc) xs (reverse indices)
