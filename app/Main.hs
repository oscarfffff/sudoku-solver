{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (forM_, replicateM)
import Data.List
import Language.Hasmtlib hiding (all)
import Prelude hiding (all, and, any, not, or, (&&), (||))

main :: IO ()
main = do
  let setCells = [(9, (0, 2)), (8, (0, 3)), (5, (0, 5)), (4, (0, 7)), (7, (0, 8)), (3, (1, 2)), (8, (1, 7)), (5, (2, 0)), (2, (2, 2)), (9, (2, 4)), (3, (3, 0)), (7, (3, 3)), (9, (3, 7)), (7, (5, 1)), (1, (5, 5)), (6, (5, 8)), (7, (6, 4)), (6, (6, 6)), (2, (6, 8)), (1, (7, 1)), (9, (7, 6)), (8, (8, 0)), (9, (8, 1)), (1, (8, 3)), (2, (8, 5)), (3, (8, 6))] -- (entry, (horizontal, vertical))
  -- let setNumbers = []
  res <- solveWith @SMT (solver $ debugging noisy z3) $ do
    setLogic "QF_LIA"
    board <- replicateM 9 $ replicateM 9 $ var @IntSort

    -- assert correct boundries for numbers
    forM_ (concat board) $ assert . (>? 0)
    forM_ (concat board) $ assert . (<? 10)

    -- assert set numbers
    forM_ setCells $ \cell -> do
      let row = fst (snd cell)
      let column = snd (snd cell)
      let n = fst cell
      assert $ ((board !! row) !! column) === n

    -- row constraint
    forM_ board $ \row -> do
      assert $ distinct row

    -- column constraint
    forM_ (transpose board) $ \column -> do
      assert $ distinct column

    -- subgrid constraint
    let subgrids = getAllSubgrids board
    forM_ subgrids $ \subgrid -> do
      assert $ distinct subgrid

    return board
  print res

-- filters a 3x3 subgrid starting at parameters row and column
getSubgridAtPosition :: [[Expr 'IntSort]] -> Int -> Int -> [Expr 'IntSort]
getSubgridAtPosition board row column = concatMap (\x -> take 3 (drop column x)) (take 3 (drop row board))

-- applies getSubgrid to the 9 different subgrids of the board:
-- (0,0), (0,3), (0,6)
-- (3,0), (3,3), (3,6)
-- (6,0), (6,3), (6,6)
getAllSubgrids :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getAllSubgrids board = concatMap (\row -> map (\column -> getSubgridAtPosition board row column) [0, 3, 6]) [0, 3, 6]
