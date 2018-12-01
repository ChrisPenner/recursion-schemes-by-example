module Examples.Algebra.FAlgebra where

import Data.Functor.Foldable

-- start snippet list-sum-algebra
sumListFAlgebra :: ListF Int Int -> Int
sumListFAlgebra (Cons val total) = val + total
sumListFAlgebra Nil              = 0

-- end snippet list-sum-algebra
