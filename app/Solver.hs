{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Solver where

import Control.Monad (forM_, replicateM)
import Data.List
import Language.Hasmtlib hiding (all)
import Prelude hiding (all, and, any, not, or, (&&), (||))

type Cell = (Int, (Int, Int))

solveSudoku :: [Cell] -> Maybe [[Expr 'IntSort]] -> IO (Result, Maybe (Decoded [[Expr 'IntSort]]))
solveSudoku predefinedValues excludedBoard = do
  solveWith @SMT (solver $ z3) $ do
    setLogic "QF_LIA"
    board <- replicateM 9 $ replicateM 9 $ var @IntSort

    -- Assert correct boundaries for numbers
    forM_ (concat board) $ assert . (>? 0)
    forM_ (concat board) $ assert . (<? 10)

    -- Assert set numbers
    forM_ predefinedValues $ \(entry, (column, row)) ->
      assert $ ((board !! column) !! row) === fromIntegral entry

    -- Row constraint
    forM_ board $ \row -> assert $ distinct row

    -- Column constraint
    forM_ (transpose board) $ \column -> assert $ distinct column

    -- Subgrid constraint
    let subgrids = getAllSubgrids board
    forM_ subgrids $ \subgrid -> do
      assert $ distinct subgrid

    -- Exclude board from solution if provided
    case excludedBoard of
      Just boardToExclude -> assert $ not $ foldl1 (&&) $ zipWith (===) (concat boardToExclude) (concat board)
      Nothing -> return ()
    return board

getSubgridAtPosition :: [[Expr 'IntSort]] -> Int -> Int -> [Expr 'IntSort]
getSubgridAtPosition board row column =
  concatMap (\x -> take 3 (drop column x)) (take 3 (drop row board))

getAllSubgrids :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getAllSubgrids board =
  concatMap (\row -> map (\column -> getSubgridAtPosition board row column) [0, 3, 6]) [0, 3, 6]
