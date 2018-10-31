{-# LANGUAGE ScopedTypeVariables #-}
module Examples.CheatSheet where

import           Data.Functor.Foldable
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M

-- start snippet cata
-- |
-- >>> sumList [1, 2, 3]
-- 6

sumList :: [Int] -> Int
sumList = cata alg
 where
  alg :: ListF Int Int -> Int
  alg Nil               = 0
  alg (Cons next total) = next + total
-- end snippet cata

-- start snippet para
-- |
-- >>> paraTails [1, 2, 3, 4]
-- [[1,2,3,4],[2,3,4],[3,4],[4]]

paraTails :: [a] -> [[a]]
paraTails = para alg
 where
  alg :: ListF a ([a], [[a]]) -> [[a]]
  alg Nil                   = []
  alg (Cons x (xs, tails')) = (x : xs) : tails'
-- end snippet para
