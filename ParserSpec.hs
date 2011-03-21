-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import ReaperProjectFileParser

parserSpecs = describe "Minimal project file" [
    it "parses successfully"
      (parse "<REAPER_PROJECT 0.1 \"3.73/OSX\">" ==
        Node {
          name = "REAPER_PROJECT",
          parameters = [ "0.1", "\"3.73/OSX\""] })
 ]

parse :: String -> Node
parse input =
  case parseProject input of
    Right r -> r

main :: IO()
main = hspec parserSpecs
