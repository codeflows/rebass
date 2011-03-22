-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (project) where

import ReaperProject(Node(Container, Leaf), Command(Command))
import Text.ParserCombinators.Parsec

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

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
    newline =
      many1 (oneOf "\n\r")

command :: CharParser st Command
command = do
  n <- name
  p <- parameters
  return $ Command n p

leafCommand :: CharParser st Node
leafCommand = do
  c <- command
  return $ Leaf c

children :: CharParser st [Node]
children = do
  endBy (node <|> leafCommand) spaces

node :: CharParser st Node
node = do
  char '<'
  c <- command
  spaces
  cs <- children
  char '>'
  return $ Container c cs

project :: CharParser st Node
project = node
