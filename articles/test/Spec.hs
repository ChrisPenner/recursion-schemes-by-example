import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Examples/CheatSheet.hs"
  , "src/Examples/Corecursive"
  , "src/Examples/Recursive"
  , "src/Examples/Tree"
  ]
