-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(
  Node(..),
  Command(..),
  parseProject) where

import Text.ParserCombinators.Parsec

data Command = Command String [String] deriving (Show, Eq)

data Node = Node Command [Command] deriving (Show, Eq)

name :: CharParser st String
name = many1 (letter <|> char '_')

parameter :: CharParser st String
parameter = many1 (noneOf " \n")

parameters :: CharParser st [String]
parameters = do
  p <- option [] parameterList
  newline
  return p
  where
    parameterList = do
      char ' '
      sepBy1 parameter (char ' ')

command :: CharParser st Command
command = do
  n <- name
  p <- parameters
  return $ Command n p

children :: CharParser st [Command]
children = do
  many (char ' ')
  return []

node :: CharParser st Node
node = do
  char '<'
  c <- command
  cs <- children
  char '>'
  return $ Node c cs

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
