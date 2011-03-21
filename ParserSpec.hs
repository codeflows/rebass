-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit
import Test.QuickCheck hiding (property)
import Test.HUnit
import Reaper

parse :: String -> String
parse = undefined

parserSpecs = describe "With '<'" [
    it "parses successfully"
      (parse "<" == "<")
  ]

main :: IO()
main = hspec parserSpecs
