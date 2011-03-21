-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit
import ReaperProjectFileParser

parserSpecs = describe "Reaper project file parser" [
    it "parses minimal project definition"
      (assertParseResult
        "<REAPER_PROJECT\n>"
        (Node "REAPER_PROJECT" [])),

    it "parses minimal project definition with parameters"
      (assertParseResult
        "<REAPER_PROJECT 0.1 \"3.73/OSX\"\n>"
        (Node "REAPER_PROJECT" ["0.1", "\"3.73/OSX\""]))
  ]

-- TODO <REAPER_PROJECT\n0.1 ... -> child nodes

assertParseResult :: String -> Node -> HUnit.Assertion
assertParseResult input expected = do
  case parseProject input of
    Left error -> HUnit.assertFailure $ show error
    Right node -> HUnit.assertEqual "parse result" expected node

main :: IO()
main = hspec parserSpecs
