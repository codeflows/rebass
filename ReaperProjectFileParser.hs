-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(Node(..), parseProject) where

import Text.ParserCombinators.Parsec

data Node = Node { name :: String
                 , parameters :: [String]
                 } deriving (Show, Eq)

name' :: CharParser st String
name' = many1 (letter <|> char '_')

parameters' :: CharParser st [String]
parameters' = undefined

node :: CharParser st Node
node = do
  char '<'
  n <- name'
  p <- parameters'
  return $ Node { name = n, parameters = [] }

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
