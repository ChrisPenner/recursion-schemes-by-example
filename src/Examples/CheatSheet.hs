module Examples.CheatSheet where

import           Data.Functor.Foldable
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M

-- |
-- >>> sumList [1, 2, 3]
-- 6

-- start snippet cata-simple
sumList :: [Int] -> Int
sumList = cata alg
 where
  alg :: ListF Int Int -> Int
  alg Nil               = 0
  alg (Cons next total) = next + total
-- end snippet cata-simple


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
