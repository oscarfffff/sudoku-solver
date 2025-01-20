{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import Bits
-- oder:
-- import Ersatz

import Control.Monad (replicateM)
import Data.Array qualified as A
import Data.Bool (bool)
import Ersatz hiding (Bits, fullAdder, halfAdder)
import Text.Printf
import Prelude hiding (all, and, any, not, or, (&&), (||))

main = knight (4, 6)

knight (h, w) = do
  out <- solveWith minisat $ do
    let bnd = ((1, 1), (h, w))
        n = A.rangeSize bnd
    a <-
      A.listArray bnd
        <$> replicateM
          n
          (unknown $ ceiling $ logBase 2 $ realToFrac n)

    assert $ flip all (A.range bnd) $ \p ->
      let s = a A.! p; t = s + encode 1
       in -- VORSICHT: so geht das nicht, muß modifiziert werden
          flip any (filter (edge (2, 1) p) (A.range bnd)) $ \q ->
            t === a A.! q

    return a
  case out of
    (Satisfied, Just a) -> putStrLn $ unlines $ do
      x <- [1 .. h]
      return $ do
        y <- [1 .. w]
        printf "%4d" $ a A.! (x, y)
