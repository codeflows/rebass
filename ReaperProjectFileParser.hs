-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(Node(Node), parseProject) where

import Text.ParserCombinators.Parsec

data Node = Node [String] deriving (Show, Eq)

name :: CharParser st String
name = many1 (letter <|> char '_')

node :: CharParser st Node
node = do
  char '<'
  n <- name
  return $ Node [n]

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
