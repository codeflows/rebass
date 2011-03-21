-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(parseNode) where

import Text.ParserCombinators.Parsec

node :: CharParser st Char
node = do
  char '<'

parseNode :: String -> Either ParseError Char
parseNode input = parse node "no source file" input
