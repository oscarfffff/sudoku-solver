{-# LANGUAGE DataKinds #-}

module Main where

import Generate
import Solver ()
import System.Random ()

main :: IO ()
main = do
  putStrLn "Creating full board..."
  board <- createCompleteBoard 10
  let indexedBoard = convertSolutionToIndexedArray $ concatMap (map fromIntegral) board
  reducedBoard <- reduceBoard indexedBoard 20
  putStrLn "------------------------------------------------------------------"
  putStrLn "New instance:"
  print reducedBoard
  putStrLn "\n Size:"
  print $ length reducedBoard
  putStrLn "------------------------------------------------------------------"
  putStrLn "Solution:"
  print board
  putStrLn "------------------------------------------------------------------"
