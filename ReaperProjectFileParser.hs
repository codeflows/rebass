-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(Node(..), parseProject) where

import Text.ParserCombinators.Parsec

data Node = Node String [String] deriving (Show, Eq)

name :: CharParser st String
name = many1 (letter <|> char '_')

parameter :: CharParser st String
parameter = many1 (noneOf " ")

parameters :: CharParser st [String]
parameters = do
  char ' '
  sepBy1 parameter (char ' ')

node :: CharParser st Node
node = do
  char '<'
  n <- name
  p <- parameters
  char '>'
  return $ Node n p

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
