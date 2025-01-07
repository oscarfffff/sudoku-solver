{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Solver where

import Control.Monad (forM_, replicateM, unless)
import Data.List
import Language.Hasmtlib hiding (all)
import Prelude hiding (all, and, any, not, or, (&&), (||))

type Cell = (Int, (Int, Int))

solveSudoku :: [Cell] -> [[Expr 'IntSort]] -> IO (Result, Maybe (Decoded [[Expr 'IntSort]]))
solveSudoku predefinedValues excludedBoard = do
  solveWith @SMT (solver $ debugging noisy z3) $ do
    setLogic "QF_LIA"
    board <- replicateM 9 $ replicateM 9 $ var @IntSort

    -- Assert correct boundaries for numbers
    forM_ (concat board) $ assert . (>? 0)
    forM_ (concat board) $ assert . (<? 10)

    -- Assert set numbers
    forM_ predefinedValues $ \(entry, (row, column)) ->
      assert $ ((board !! row) !! column) === fromIntegral entry

    -- Row constraint
    forM_ board $ \row -> do
      assert $ distinct row

    -- Column constraint
    forM_ (transpose board) $ \column -> do
      assert $ distinct column

    -- Subgrid constraint
    let subgrids = getAllSubgrids board
    forM_ subgrids $ \subgrid -> do
      assert $ distinct subgrid

    unless (length excludedBoard == 0) $ do
      assert $ not $ foldl1 (&&) $ zipWith (===) (concat $ excludedBoard) (concat board)

    return board

-- Filters a 3x3 subgrid starting at parameters row and column
getSubgridAtPosition :: [[Expr 'IntSort]] -> Int -> Int -> [Expr 'IntSort]
getSubgridAtPosition board row column = concatMap (\x -> take 3 (drop column x)) (take 3 (drop row board))

-- Applies getSubgrid to the 9 different subgrids of the board:
-- (0,0), (0,3), (0,6)
-- (3,0), (3,3), (3,6)
-- (6,0), (6,3), (6,6)
getAllSubgrids :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getAllSubgrids board = concatMap (\row -> map (\column -> getSubgridAtPosition board row column) [0, 3, 6]) [0, 3, 6]
