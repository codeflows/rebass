-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(Node(..), parseProject) where

import Text.ParserCombinators.Parsec

data Node = Node { name :: String
                 , parameters :: [String]
                 } deriving (Show, Eq)

nameP :: CharParser st String
nameP = many1 (letter <|> char '_')

node :: CharParser st Node
node = do
  char '<'
  n <- nameP
  return $ Node { name = n, parameters = [] }

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
