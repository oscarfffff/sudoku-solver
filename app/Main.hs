{-# LANGUAGE DataKinds #-}

module Main where

import Generate
import Utils
import Solver ()
import System.Random ()

main :: IO ()
main = do
  board <- createCompleteBoard 10
  reducedBoard <- reduceBoard (convertSolutionToIndexedArray $ concatMap (map fromIntegral) board) 15
  putStrLn "------------------------------------------------------------------"
  putStrLn "New instance: \n"
  print $ createGrid reducedBoard
  putStrLn "\n Size:"
  print $ length reducedBoard
  putStrLn "------------------------------------------------------------------"
  putStrLn "Solution: \n"
  displayGrid board
  putStrLn "------------------------------------------------------------------"
