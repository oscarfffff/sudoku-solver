module Utils where

import Data.Maybe (fromMaybe)

createGrid :: [(Int, (Int, Int))] -> [[Char]]
createGrid positions = [[fromMaybe '*' (lookup (x, y) posMap >>= (Just . head . show)) | y <- [0..8]] | x <- [0..8]]
  where posMap = [((r, c), n) | (n, (r, c)) <- positions]


displayGrid :: [[Integer]] -> IO ()
displayGrid grid = mapM_ putStrLn $ map formatRow grid
  where
    formatRow row = unwords $ map show row
