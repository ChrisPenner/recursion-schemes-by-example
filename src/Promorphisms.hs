{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Promorphisms where

import           Data.Functor.Foldable
import           JSON
import qualified Data.Map                      as M



filterNulls :: JSONF JSON -> JSONF JSON
filterNulls (ArrayF  arr) = ArrayF (filter (not . isNull) arr)
filterNulls (ObjectF obj) = ObjectF (M.filter (not . isNull) obj)
filterNulls j             = j

isNull :: JSON -> Bool
isNull Null = True
isNull _    = False
