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
      "<REAPER_PROJECT 0.1 \"3.73/OSX\"\n>" `shouldParseInto` emptyReaperProjectWithVersions,

    it "accepts carriage returns instead of newlines" $
      "<REAPER_PROJECT 0.1 \"3.73/OSX\"\r>" `shouldParseInto` emptyReaperProjectWithVersions,

    it "parses project definition with one command" $
      "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n>" `shouldParseInto`
        (emptyReaperProjectHeader [Command "SAMPLERATE" [Integer 44100, Integer 0]]),

    it "parses project definition with many commands" $
      "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n  LOCK 1\n>" `shouldParseInto` projectDefinitionWithManyCommands,

    it "parses project definition with many commands regardless of whitespace" $
      "<REAPER_PROJECT\nSAMPLERATE 44100 0\nLOCK 1\n>" `shouldParseInto` projectDefinitionWithManyCommands,

    it "parses project definition with child nodes" $
      "<REAPER_PROJECT\n  <CHILD 1\n    CHILD_COMMAND 2\n  >\n>" `shouldParseInto` projectDefinitionWithChildContainers,

    it "parses project definition with child nodes regardless of whitespace" $
      "<REAPER_PROJECT\n<CHILD 1\nCHILD_COMMAND 2\n>\n>" `shouldParseInto` projectDefinitionWithChildContainers,

    it "parses dense project definition with three levels of nodes" $
      "<REAPER_PROJECT\n<A\n<B\n<C\n>>>>" `shouldParseInto`
        emptyReaperProjectHeader [
          Container "A" [] [
            Container "B" [] [
              Container "C" [] []
            ]
          ]
        ],

    it "accepts digits in command names" $
      "<REAPER_PROJECT\nRENDER_1X 0\n>" `shouldParseInto` emptyReaperProjectHeader [Command "RENDER_1X" [Integer 0]],

    it "parses string literals" $
      "<REAPER_PROJECT\n  MARKER 2 31.30434782608696 \"Verse 1\" 0\n>" `shouldParseInto`
        emptyReaperProjectHeader [
          Command "MARKER" [Integer 2, Decimal "31.30434782608696", String "Verse 1", Integer 0]
        ],

    it "parses string literals with single quotes" $
      "<REAPER_PROJECT\nMARKER 1 1.00000000000000 'The \"Hit\" Chorus' 0\n" `shouldParseInto`
        emptyReaperProjectHeader [
          Command "MARKER" [Integer 1, Decimal "1.00000000000000", String "The \"Hit\" Chorus", Integer 0]
        ],

    it "parses negative decimals" $
      "<REAPER_PROJECT -0.1\n>" `shouldParseInto` reaperProjectHeader [Decimal "-0.1"] [],

    it "parses negative integers" $
      "<REAPER_PROJECT -10\n>" `shouldParseInto` reaperProjectHeader [Integer (-10)] []
  ]

projectDefinitionWithManyCommands =
  emptyReaperProjectHeader [
    Command "SAMPLERATE" [Integer 44100, Integer 0],
    Command "LOCK" [Integer 1]
  ]

projectDefinitionWithChildContainers =
  emptyReaperProjectHeader [
    Container "CHILD" [Integer 1] [
      Command "CHILD_COMMAND" [Integer 2]
    ]
  ]

reaperProjectHeader = Container "REAPER_PROJECT"
emptyReaperProjectHeader = reaperProjectHeader []
emptyReaperProjectWithVersions = reaperProjectHeader [Decimal "0.1", String "3.73/OSX"] []
emptyReaperProject = emptyReaperProjectHeader []

shouldParseInto :: String -> Node -> HUnit.Assertion
shouldParseInto input expected = do
  case parseProject input of
    Left error -> HUnit.assertFailure $ show error
    Right node -> HUnit.assertEqual "parse result" expected node

parseProject :: String -> Either ParseError Node
parseProject input = parse project "(no source file)" input

main :: IO()
main = hspec parserSpecs
