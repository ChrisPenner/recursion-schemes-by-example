{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Recursive where

import           Data.Functor.Foldable
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M

------------------ Cata -----------------------
-- sum list

sumList :: [Int] -> Int
sumList = cata alg
 where
  alg :: ListF Int Int -> Int
  alg Nil               = 0
  alg (Cons next total) = next + total

sumTree :: Tree Int -> Int
sumTree = cata alg
 where
  alg :: TreeF Int Int -> Int
  alg (LeafF n    ) = n
  alg (BranchF a b) = a + b

sumJSON :: JSON -> Double
sumJSON = cata alg
 where
  alg :: JSONF Double -> Double
  alg NullF         = 0
  alg (ObjectF obj) = sum obj
  alg (ArrayF  arr) = sum arr
  alg (StringF s  ) = 0
  alg (NumberF n  ) = n
  alg (BoolF   b  ) = 0

mapList :: forall a b . (a -> b) -> [a] -> [b]
mapList f = cata alg
 where
  alg :: ListF a [b] -> [b]
  alg Nil         = []
  alg (Cons a bs) = f a : bs

mapTree :: forall a b . (a -> b) -> Tree a -> Tree b
mapTree f = cata alg
 where
  alg :: TreeF a (Tree b) -> Tree b
  alg (LeafF a    ) = Leaf (f a)
  alg (BranchF l r) = Branch l r

mapNumbers :: (Double -> Double) -> JSON -> JSON
mapNumbers f = cata alg
 where
  alg :: JSONF JSON -> JSON
  alg (NumberF n) = Number (f n)
  alg j           = embed j


-- |
-- >>> let nestedJSON = jsonFromString $ "{ \"a\": [1,2,3], \"b\": { \"c\": \"hello\" } }"
-- >>> flattenJSON nestedJSON
-- [Number 1.0,Number 2.0,Number 3.0,String "hello"]

-- start snippet cata-advanced
flattenJSON :: JSON -> [JSON]
flattenJSON = cata alg
 where
  alg :: JSONF [JSON] -> [JSON]
  alg NullF         = [Null]
  alg (ObjectF obj) = concat . M.elems $ obj
  alg (ArrayF  arr) = concat arr
  alg (StringF s  ) = [String s]
  alg (NumberF n  ) = [Number n]
  alg (BoolF   b  ) = [Bool b]
-- end snippet cata-advanced

flattenObjects :: JSON -> JSON
flattenObjects = cata alg
 where
  alg :: JSONF JSON -> JSON
  alg (ObjectF obj) = Object (M.foldMapWithKey go obj)
   where
    go k (Object obj) = M.mapKeys (\p -> k <> "." <> p) obj
    go k (Array arr) =
      let indexes = show <$> [0 ..] in M.fromList (zip indexes arr)
    go k v = M.singleton k v
  alg (ArrayF arr) =
    let indexes = show <$> [0 ..] in Object $ M.fromList (zip indexes arr)
  alg NullF       = Null
  alg (StringF s) = String s
  alg (NumberF n) = Number n
  alg (BoolF   b) = Bool b


-- Calculate a rolling average from the end of the list to the start.
-- e.g. The first element of the resulting list is the 
rollingAverage :: [Float] -> [Float]
rollingAverage = para alg
 where
  alg :: ListF Float ([Float], [Float]) -> [Float]
  alg Nil = []
  alg (Cons n (rest, avgs)) =
    let total = sum (n : rest)
        len   = fromIntegral (length (n : rest))
    in  (total / len) : avgs
