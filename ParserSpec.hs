-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit

import ReaperProject
import ReaperProjectFileParser

parserSpecs = describe "Reaper project file parser" [
    it "parses minimal project definition"
      (assertParseResult
        "<REAPER_PROJECT\n>"
        (Container
          (Command "REAPER_PROJECT" [])
          [])),

    it "parses minimal project definition with parameters"
      (assertParseResult
        "<REAPER_PROJECT 0.1 \"3.73/OSX\"\n>"
        (Container
          (Command "REAPER_PROJECT" ["0.1", "\"3.73/OSX\""])
          [])),

    it "parses project definition with one command"
      (assertParseResult
        "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n>"
        (Container
          (Command "REAPER_PROJECT" [])
          [Leaf $ Command "SAMPLERATE" ["44100", "0"]])),

   it "parses project definition with many commands"
      (assertParseResult
        "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n  LOCK 1\n>"
        (Container
          (Command "REAPER_PROJECT" [])
          [Leaf $ Command "SAMPLERATE" ["44100", "0"], Leaf $ Command "LOCK" ["1"]])),

   it "parses project definition with many commands regardless of whitespace"
      (assertParseResult
        "<REAPER_PROJECT\nSAMPLERATE 44100 0\nLOCK 1\n>"
        (Container
          (Command "REAPER_PROJECT" [])
          [Leaf $ Command "SAMPLERATE" ["44100", "0"], Leaf $ Command "LOCK" ["1"]])),

    it "parses project definition with child nodes"
      (assertParseResult
        "<REAPER_PROJECT\n  <CHILD 1\n    CHILD_COMMAND 2\n  >\n>"
        projectDefinitionWithChildren
      ),

    it "parses project definition with child nodes regardless of whitespace"
      (assertParseResult
        "<REAPER_PROJECT\n<CHILD 1\nCHILD_COMMAND 2\n>\n>"
        projectDefinitionWithChildren
      )
  ]

projectDefinitionWithChildren =
  Container (Command "REAPER_PROJECT" [])
    [
      Container (Command "CHILD" ["1"])
        [
          Leaf (Command "CHILD_COMMAND" ["2"])
        ]
    ]

{- TODO parameter types:
 - integers: 6 -1
 - decimals: 1.000000000 -1.00000
 - strings:  "mah string"
 -}
-- TODO failure cases

assertParseResult :: String -> Node -> HUnit.Assertion
assertParseResult input expected = do
  case parseProject input of
    Left error -> HUnit.assertFailure $ show error
    Right node -> HUnit.assertEqual "parse result" expected node

main :: IO()
main = hspec parserSpecs
