{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Examples.Corecursive where

import           Data.Functor.Foldable
import           Data.Functor.Base
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.List
import           Control.Monad

------------------ Ana -----------------------

buildList :: Int -> [Int]
buildList = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 0 = Nil
  coalg n = Cons n (n - 1)

tails :: String -> [String]
tails = ana coalg
 where
  coalg :: String -> ListF String String
  coalg ""         = Nil
  coalg s@(_ : xs) = Cons s xs

mapList :: forall a b . (a -> b) -> [a] -> [b]
mapList f = ana coalg
 where
  coalg :: [a] -> ListF b [a]
  coalg []       = Nil
  coalg (x : xs) = Cons (f x) xs

mapTree :: forall a b . (a -> b) -> Tree a -> Tree b
mapTree f = ana coalg
 where
  coalg :: Tree a -> TreeF b (Tree a)
  coalg (Leaf a    ) = LeafF (f a)
  coalg (Branch l r) = BranchF l r

mapNumbers :: (Double -> Double) -> JSON -> JSON
mapNumbers f = ana alg
 where
  alg :: JSON -> JSONF JSON
  alg (Number n) = NumberF (f n)
  alg j          = project j

buildPath :: ([String], Double) -> JSON
buildPath = ana coalg
 where
  coalg :: ([String], Double) -> JSONF ([String], Double)
  coalg ([]       , d) = NumberF d
  coalg (p : paths, d) = ObjectF (M.singleton p (paths, d))


collatzBuilder :: Int -> [Int]
collatzBuilder = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 0 = Nil
  coalg 1 = Cons 1 0
  coalg n | even n    = Cons n (n `div` 2)
          | otherwise = Cons n ((n * 3) + 1)

anaGroupBy :: Int -> [a] -> [[a]]
anaGroupBy n = ana coalg
 where
  coalg :: [a] -> ListF [a] [a]
  coalg [] = Nil
  coalg xs = Cons (take n xs) (drop n xs)

anaPrimeFactors :: Int -> [Int]
anaPrimeFactors = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 1 = Nil
  coalg n =
    let nextFactor = fromMaybe n $ find ((== 0) . mod n) [2 ..]
    in  Cons nextFactor (n `div` nextFactor)

displayFactors :: Int -> String
displayFactors = hylo alg coalg
 where
  alg :: NonEmptyF Int String -> String
  alg (NonEmptyF n Nothing    ) = show n
  alg (NonEmptyF n (Just rest)) = show n ++ " * " ++ rest
  coalg :: Int -> NonEmptyF Int Int
  coalg 1 = NonEmptyF 1 Nothing
  coalg n =
    let nextFactor    = fromMaybe n $ find ((== 0) . mod n) [2 ..]
        reducedNumber = n `div` nextFactor
    in  NonEmptyF nextFactor
          $ if reducedNumber == 1 then Nothing else Just reducedNumber

-- |
-- >>> apoMapFirst even (*100) [1, 2, 3, 4]
-- [1,200,3,4]
apoMapFirst :: forall a . (a -> Bool) -> (a -> a) -> [a] -> [a]
apoMapFirst predicate f = apo coalg
 where
  coalg :: [a] -> ListF a (Either [a] [a])
  coalg [] = Nil
  coalg (x : xs) | predicate x = Cons (f x) (Left xs)
                 | otherwise   = Cons x (Right xs)


apoArbitraryPrecisionSqrt :: Float -> [Float]
apoArbitraryPrecisionSqrt = apo coalg
 where
  coalg n =
    let next = sqrt n
    in  if abs (next - n) < 0.5
          then Cons n (Left [next])
          else Cons n (Right next)
