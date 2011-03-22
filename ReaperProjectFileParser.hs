-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (parseProject) where

import ReaperProject(Node(Container, Leaf), Command(Command))
import Text.ParserCombinators.Parsec

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

leafCommand :: CharParser st Node
leafCommand = do
  c <- command
  return $ Leaf c

child :: CharParser st Node
child = do
  spaces
  node <|> leafCommand

node :: CharParser st Node
node = do
  char '<'
  c <- command
  -- TODO consume whitespace here anyway
  cs <- many child
  char '>'
  return $ Container c cs

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
