-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit
import ReaperProjectFileParser

parserSpecs = describe "Reaper project file parser" [
{-    it "parses minimal project definition"
      (parse "<REAPER_PROJECT>" ==
        Node {
          name = "REAPER_PROJECT",
          parameters = [ "0.1", "\"3.73/OSX\""] })
 ]-}
    it "parses successfully"
      (do
         let expected = Node "REAPER_PROJECT" ["0.1", "\"3.73/OSX\""]
         let actual   = parse "<REAPER_PROJECT 0.1 \"3.73/OSX\">" 
         HUnit.assertEqual "letters to numbers" expected actual)

--      (parse "<REAPER_PROJECT 0.1 \"3.73/OSX\">" ==
--        Node "REAPER_PROJECT" ["0.1", "\"3.73/OSX\""])
  ]

-- TODO <REAPER_PROJECT\n0.1 ... -> child nodes

parse :: String -> Node
parse input =
  case parseProject input of
    Right r -> r

main :: IO()
main = hspec parserSpecs
