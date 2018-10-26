{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Examples where

import           Aeson
import           JSON

import qualified Data.Aeson                    as A
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree  as CF
                                                          ( CofreeF(..) )
import qualified Data.Map                      as M
import           Data.Functor.Foldable
import           Control.Monad.Free
import           Data.Bifunctor

import           Data.Monoid

pathsCata :: JSON -> Cofree JSONF [String]
pathsCata = cata addPath
 where
  addPath :: JSONF (Cofree JSONF [String]) -> Cofree JSONF [String]
  addPath (ObjectF o) = [] :< ObjectF (M.mapWithKey addKey o)
    where addKey k v = (k :) <$> v
  addPath (ArrayF a) = [] :< ArrayF (zipWith addIndex a [0 ..])
    where addIndex a i = (show i :) <$> a
  addPath (StringF s) = [] :< StringF s
  addPath (NumberF n) = [] :< NumberF n
  addPath (BoolF   b) = [] :< BoolF b
  addPath NullF       = [] :< NullF

pathsCata' :: JSON -> Cofree JSONF [String]
pathsCata' j = cata addPath j []
 where
  addPath
    :: JSONF ([String] -> Cofree JSONF [String])
    -> ([String] -> Cofree JSONF [String])
  addPath (ObjectF o) p = p :< ObjectF (M.mapWithKey addKey o)
    where addKey k v = v (p ++ [k])
  addPath (ArrayF a) p = [] :< ArrayF (zipWith addIndex a [0 ..])
    where addIndex a i = a (p ++ [show i])
  addPath (StringF s) p = p :< StringF s
  addPath (NumberF n) p = p :< NumberF n
  addPath (BoolF   b) p = p :< BoolF b
  addPath NullF       p = p :< NullF


pathsAna :: JSON -> Cofree JSONF [String]
pathsAna = ana addPath . ([], )
 where
  addPath :: ([String], JSON) -> CF.CofreeF JSONF [String] ([String], JSON)
  addPath (p, Object obj) = p CF.:< ObjectF (M.mapWithKey addKey obj)
    where addKey k v = (p ++ [k], v)
  addPath (p, Array arr) = p CF.:< ArrayF (zip paths arr)
   where
    paths = do
      n <- [0 ..]
      return (p ++ [show n])
  addPath (p, String s) = p CF.:< StringF s
  addPath (p, Number n) = p CF.:< NumberF n
  addPath (p, Bool b  ) = p CF.:< BoolF b
  addPath (p, Null    ) = p CF.:< NullF
