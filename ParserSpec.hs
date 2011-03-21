-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit
import Test.QuickCheck hiding (property)
import Test.HUnit
import ReaperProjectFileParser

parse :: String -> Char
parse input =
  case parseNode input of
    Right r -> r

parserSpecs = describe "With '<'" [
    it "parses successfully"
      (parse "<" == '<')
  ]

main :: IO()
main = hspec parserSpecs
