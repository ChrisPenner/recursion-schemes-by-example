{-# LANGUAGE ScopedTypeVariables #-}
module Examples.CheatSheet where

import           Data.Functor.Foldable
import           Data.Functor.Base
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Control.Comonad.Cofree        as CF
import           Control.Comonad
import           Data.List                     as L
import Data.Maybe

-- |
-- start snippet project
-- project swaps the first layer of your type with the recursive functor
-- version leaving the next layer untouched
-- >>> project [1, 2, 3]
-- Cons 1 [2,3]

-- end snippet project


-- |
-- start snippet cata
-- >>> sumList [1, 2, 3]
-- 6

sumList :: [Int] -> Int
sumList = cata alg
 where
  alg :: ListF Int Int -> Int
  alg Nil               = 0
  alg (Cons next total) = next + total
-- end snippet cata

-- |
-- start snippet para
-- >>> paraTails [1, 2, 3, 4]
-- [[1,2,3,4],[2,3,4],[3,4],[4]]

paraTails :: [a] -> [[a]]
paraTails = para alg
 where
  alg :: ListF a ([a], [[a]]) -> [[a]]
  alg Nil                   = []
  alg (Cons x (xs, tails')) = (x : xs) : tails'
-- end snippet para

-- |
-- start snippet zygo
-- >>> zygoDedupeList [1, 1, 3, 2, 3]
-- [1,2,3]

zygoDedupeList :: [Int] -> [Int]
zygoDedupeList = zygo setBuilderAlg listBuilderAlg
 where
  setBuilderAlg :: ListF Int (S.Set Int) -> S.Set Int
  setBuilderAlg Nil              = S.empty
  setBuilderAlg (Cons n allNums) = S.insert n allNums
  listBuilderAlg :: ListF Int (S.Set Int, [Int]) -> [Int]
  listBuilderAlg Nil = []
  listBuilderAlg (Cons n (allNums, ns)) =
    if S.member n allNums then ns else n : ns
-- end snippet zygo

-- |
-- start snippet histo
-- ???
-- histoDenestArrays :: JSON -> JSON
-- histoDenestArrays = histo alg
--  where
--   alg :: JSONF (CF.Cofree JSONF JSON) -> JSON
--   alg NullF         = Null
--   alg (StringF s  ) = String s
--   alg (BoolF   b  ) = Bool b
--   alg (NumberF n  ) = Number n
--   alg (ArrayF  arr) = Array (foldMap flatten arr)
--    where
--     flatten :: CF.Cofree JSONF JSON -> [JSON]
--     flatten (_ CF.:< ArrayF arr) = extract <$> arr
--     flatten w                    = [extract w]
--   alg (ObjectF obj) = Object (extract <$> obj)
-- end snippet histo



-- Corecursive

-- |
-- start snippet embed
-- embed converts a top layer of your recursive functor type into its companion
-- type by 'embedding' the recursive portions within.
-- >>> embed (Cons 1 [2, 3])
-- [1,2,3]

-- end snippet embed


-- |
-- start snippet ana
-- >>> anaCountDown 5
-- [5,4,3,2,1]

anaCountDown :: Int -> [Int]
anaCountDown = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 0 = Nil
  coalg n = Cons n (n - 1)
-- end snippet ana

-- |
-- start snippet apo
-- >>> apoConcat [1, 2] [3, 4]
-- [1,2,3,4]

apoConcat :: forall a . [a] -> [a] -> [a]
apoConcat xs ys = apo coalg xs
 where
  coalg :: [a] -> ListF a (Either [a] [a])
  -- Recurse until the end of xs
  coalg (x : xs) = Cons x (Right xs)
  -- Upon hitting the end of 'xs', append ys, but use 'Left' to NOT recurse
  coalg []       = Left <$> project ys
-- end snippet apo

-- |
-- start snippet hylo
-- >>> hyloDisplayFactors 30
-- "2 * 3 * 5"
hyloDisplayFactors :: Int -> String
hyloDisplayFactors = hylo alg coalg
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
-- end snippet hylo
