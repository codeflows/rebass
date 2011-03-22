-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (parseProject) where

import ReaperProject
import Text.ParserCombinators.Parsec

parseName :: CharParser st String
parseName = many1 (letter <|> char '_')

parameter :: CharParser st String
parameter = many1 (noneOf " \n")

parseParameters :: CharParser st [String]
parseParameters = do
  p <- option [] parameterList
  newline
  return p
  where
    parameterList = do
      char ' '
      sepBy1 parameter (char ' ')

parseCommand :: CharParser st Command
parseCommand = do
  n <- parseName
  p <- parseParameters
  return $ Command n p

-- TODO this should return Nodes
parseChildren :: CharParser st [Command]
parseChildren = many commands
  where
    commands = do
      many (char ' ')
      parseCommand
      -- TODO parse commands OR nodes here

node :: CharParser st Node
node = do
  char '<'
  c <- parseCommand
  cs <- parseChildren
  char '>'
  return $ Container c cs

parseProject :: String -> Either ParseError Node
parseProject input = parse node "(no source file)" input
