{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Examples.Tree where

import Data.Functor.Foldable
import           Control.Comonad.Cofree
import         qualified  Control.Comonad.Trans.Cofree  as CFT (CofreeF(..))
import Control.Monad.Free
import Control.Comonad
import Data.Foldable
import Data.Functor.Base
import Control.Applicative

type RoseTree a = Cofree [] a
--          1
--       2     3
--      4 5   6 7
nestedTree :: RoseTree Int
nestedTree = 1 :< [2 :< [4 :< [], 5 :< []], 3 :< [6 :< [], 7 :< []]]

-- | 
-- >>> depthFirst nestedTree
-- [1,2,4,5,3,6,7]
depthFirst :: RoseTree a -> [a]
depthFirst = cata alg
 where
  alg :: CFT.CofreeF [] a [a] -> [a]
  alg (a CFT.:< children) = a : concat children


-- | 
-- >>> breadthFirst nestedTree
-- [1,2,3,4,5,6,7]
breadthFirst :: RoseTree a -> [a]
breadthFirst = concat @[] . ana coalg . pure
 where
  coalg :: [Cofree [] a] -> ListF [a] [Cofree [] a]
  coalg [] = Nil
  coalg xs =
    let (as, rest) = foldMap (\(a :< children) -> ([a], children)) xs
    in  Cons as rest


-- | 
-- >>> breadthFirst nestedTree
-- [1,2,3,4,5,6,7]
-- futuBreadthFirst :: RoseTree a -> [a]
-- futuBreadthFirst = futu coalg . pure
--  where
--   coalg
--     :: NonEmpty (RoseTree a) -> ListF a (Free (ListF a) (NundefinedonEmpty (RoseTree a)))
--   coalg trees =
--     let (heads, rests) = foldMap (\(a :< children) -> ([a], children)) trees
--     in  

-- ZipList Breadth First

zippyBreadthFirst :: forall a . RoseTree a -> [a]
zippyBreadthFirst = concat . cata alg
 where
  alg :: CFT.CofreeF [] a [[a]] -> [[a]]
  alg (a CFT.:< []) = []
  alg (a CFT.:< children) =
    [a]
      : foldl' (\acc next -> zipWith mappend acc (next <> repeat []))
               (repeat [])
               children
