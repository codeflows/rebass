-- http://hackage.haskell.org/packages/archive/hspec/0.3.0/doc/html/Test-Hspec.html

module ParserSpec where

import Test.Hspec
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit

import Text.ParserCombinators.Parsec

import ReaperProject
import ReaperProjectFileParser

parserSpecs = describe "Reaper project file parser" [
    it "parses minimal project definition" $
      "<REAPER_PROJECT\n>" `shouldParseInto` emptyReaperProject,

    it "parses minimal project definition with parameters" $
      "<REAPER_PROJECT 0.1 \"3.73/OSX\"\n>" `shouldParseInto` (reaperProjectHeader ["0.1", "\"3.73/OSX\""] []),

    it "parses project definition with one command" $
      "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n>" `shouldParseInto`
        (emptyReaperProjectHeader [Leaf (Command "SAMPLERATE" ["44100", "0"])]),

    it "parses project definition with many commands" $
      "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n  LOCK 1\n>" `shouldParseInto` projectDefinitionWithManyCommands,

    it "parses project definition with many commands regardless of whitespace" $
      "<REAPER_PROJECT\nSAMPLERATE 44100 0\nLOCK 1\n>" `shouldParseInto` projectDefinitionWithManyCommands,

    it "parses project definition with child nodes" $
      "<REAPER_PROJECT\n  <CHILD 1\n    CHILD_COMMAND 2\n  >\n>" `shouldParseInto` projectDefinitionWithChildContainers,

    it "parses project definition with child nodes regardless of whitespace" $
      "<REAPER_PROJECT\n<CHILD 1\nCHILD_COMMAND 2\n>\n>" `shouldParseInto` projectDefinitionWithChildContainers,

    it "parses dense project definition with three levels of nodes" $
      "<REAPER_PROJECT\n<A\n<B\n<C D\n>>>>" `shouldParseInto`
      (emptyReaperProjectHeader
        [
          Container (Command "A" [])
          [
            Container (Command "B" [])
            [
              Container (Command "C" ["D"])
              []
            ]
          ]
        ]
      ),

    it "accepts digits in command names" $
      "<REAPER_PROJECT\nRENDER_1X 0\n>" `shouldParseInto` (emptyReaperProjectHeader [Leaf (Command "RENDER_1X" ["0"])]),

    it "accepts carriage returns instead of newlines" $
      "<REAPER_PROJECT\r>" `shouldParseInto` emptyReaperProject
  ]

projectDefinitionWithManyCommands =
  emptyReaperProjectHeader
    [
      Leaf (Command "SAMPLERATE" ["44100", "0"]),
      Leaf (Command "LOCK" ["1"])
    ]

projectDefinitionWithChildContainers =
  emptyReaperProjectHeader
    [
      Container (Command "CHILD" ["1"])
        [
          Leaf (Command "CHILD_COMMAND" ["2"])
        ]
    ]

reaperProjectHeader parameters = Container (Command "REAPER_PROJECT" parameters)
emptyReaperProjectHeader = reaperProjectHeader []
emptyReaperProject = emptyReaperProjectHeader []

{- TODO parameter types:
 - integers: 6 -1
 - decimals: 1.000000000 -1.00000
 - strings:  "mah string"
 -}
-- TODO failure cases
-- TODO require that top-level tag actually is REAPER_PROJECT?

shouldParseInto :: String -> Node -> HUnit.Assertion
shouldParseInto input expected = do
  case parseProject input of
    Left error -> HUnit.assertFailure $ show error
    Right node -> HUnit.assertEqual "parse result" expected node

parseProject :: String -> Either ParseError Node
parseProject input = parse project "(no source file)" input

main :: IO()
main = hspec parserSpecs
