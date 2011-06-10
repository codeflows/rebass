{-
 - hspec specs for the Reaper project file parser.
 -
 - TODO don't repeat the <REAPER_PROJECT parsing in each spec
 - TODO handle data chunks:
 -    <AU "AUi: My Plugin" "My Plugin" ""
 -      6QMAAAAAAAAAAAAAEAAAAAEAAAAAAAAAAgAAAAAAAAAAAAAAAAAAAA==
 -      ...
 - TODO handle MIDI stuff:
 -      <X 0 0
 -        /wNFREZfTShfRzBfMTVS
 -      >
 -}

module Main where

import Test.Hspec
import Test.Hspec.HUnit ()
import qualified Test.HUnit as HUnit

import Text.ParserCombinators.Parsec

import Rebass.Reaper.ReaperProject
import Rebass.Reaper.ReaperProjectFileParser

parserSpecs = describe "Reaper project file parser" [
    it "parses minimal project definition" $
      "<REAPER_PROJECT\n>" `shouldParseInto` emptyReaperProject,

    it "parses minimal project definition with parameters" $
      "<REAPER_PROJECT 0.1 \"3.73/OSX\"\n>" `shouldParseInto` emptyReaperProjectWithVersions,

    it "accepts carriage returns instead of newlines" $
      "<REAPER_PROJECT 0.1 \"3.73/OSX\"\r>" `shouldParseInto` emptyReaperProjectWithVersions,

    it "parses project definition with one command" $
      "<REAPER_PROJECT\n  SAMPLERATE 44100 0\n>" `shouldParseInto`
        emptyReaperProjectHeader [Command "SAMPLERATE" [Integer 44100, Integer 0]],

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
      "<REAPER_PROJECT\nMARKER 2 31.30434782608696 \"Verse 1\" 0\n>" `shouldParseInto`
        emptyReaperProjectHeader [
          Command "MARKER" [Integer 2, Decimal "31.30434782608696", String "Verse 1", Integer 0]
        ],

    it "parses string literals with single quotes" $
      "<TRACK '{70223F50-ACF7-7F3A-758F-BD0AD38ACDCD}'\nNAME '\"Awesome\" shredding'\n>" `shouldParseInto`
        Container "TRACK" [String "{70223F50-ACF7-7F3A-758F-BD0AD38ACDCD}"]
          [Command "NAME" [String "\"Awesome\" shredding"]],

    it "parses string literals with back ticks" $
      "<TRACK\nNAME `\"O\" 'RLY?'`\n>" `shouldParseInto`
        Container "TRACK" []
          [Command "NAME" [String "\"O\" 'RLY?'"]],

    it "parses unquoted GUIDs" $
      "<TRACK '{A8C514FE-5292-859C-A662-E4A93B58A873}'\nTRACKID {A8C514FE-5292-859C-A662-E4A93B58A873}\n>"
        `shouldParseInto`
          Container "TRACK" [String "{A8C514FE-5292-859C-A662-E4A93B58A873}"]
            [Command "TRACKID" [String "{A8C514FE-5292-859C-A662-E4A93B58A873}"]],

    it "parses string identifier parameters" $
      "<SOURCE WAVE\nFILE \"02-110328_2314.wav\"\n>" `shouldParseInto`
        Container "SOURCE" [String "WAVE"]
          [Command "FILE" [String "02-110328_2314.wav"]],

    it "parses negative decimals" $
      "<REAPER_PROJECT -0.1\n>" `shouldParseInto` reaperProjectHeader [Decimal "-0.1"] [],

    it "parses negative integers" $
      "<REAPER_PROJECT -10\n>" `shouldParseInto` reaperProjectHeader [Integer (-10)] [],

    it "consumes all input" $
      parsingShouldFailFor "<REAPER_PROJECT -10\n><hallo>",

    -- TODO new Node type for data chunk?
    it "parses containers with data chunks" $
      "<RENDER_CFG\nbDNwbcAAAAABAAAA//////////8EAAAAwAAAAAAAAAA=\n>" `shouldParseInto`
        Container "RENDER_CFG" [] [],

    it "parses containers with parameters and data chunks" $
      ("<VST 'VST: ReaVerb (Cockos)' 'reaverb.vst.dylib' 0 ''\n" ++
       " YnZlcu5e7f4CAAAAAQAAAAAAAAACAAAAAAAAAAIAAAABAAAAAAAAAA==\n" ++
       " AgAAAAAAAACBAAAAAQAAAAAAAAAAAODAtvNuPgAAgD8AAAAAAACAPw==\n" ++
       " AACAP6rxkj4AAAAARUNIT0dFTgAcAAAAKUyUQhDdZkSu6PJAKc4xPw==\n" ++
       " Q9LPOwZGZ0AAAAA/RUNIT0dFTgAcAAAAAACgQQAAIEIAAPBBVUJ/Pw==\n" ++
       " bxZsPwAAAAAAAAA/RklMVAAIAAAAW3tJPolA5Ds=\n>") `shouldParseInto`
        Container "VST" [] []
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
shouldParseInto input expected =
  case parseProject input of
    Left error -> HUnit.assertFailure $ show error
    Right node -> HUnit.assertEqual "parse result" expected node

parsingShouldFailFor :: String -> HUnit.Assertion
parsingShouldFailFor input =
  case parseProject input of
    Left error -> return ()
    Right node -> HUnit.assertFailure "Parsing should have failed"

parseProject :: String -> Either ParseError Node
parseProject = parse project "(no source file)"

main :: IO()
main = hspecX parserSpecs
