-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (project) where

import ReaperProject(Node(Container, Command))
import Text.ParserCombinators.Parsec

data NameAndParameters = NameAndParameters { name' :: String, parameters' :: [String] }

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

parameter :: CharParser st String
parameter =
      -- TODO this will probably accepts newlines inside quotes as well
      between quote quote (many $ noneOf "\"")
      -- TODO duplication
  <|> many1 (noneOf " \n\r")
      where
        quote = char '"'

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
      -- TODO not exactly correct, should be a combination of these
      many1 (oneOf "\n\r")

nameAndParameters :: CharParser st NameAndParameters
nameAndParameters = do
  n <- name
  p <- parameters
  return $ NameAndParameters n p

command :: CharParser st Node
command = do
  np <- nameAndParameters
  return $ Command (name' np) (parameters' np)

children :: CharParser st [Node]
children = do
  endBy (node <|> command) spaces

node :: CharParser st Node
node = do
  char '<'
  np <- nameAndParameters
  spaces
  cs <- children
  char '>'
  return $ Container (name' np) (parameters' np) cs

project :: CharParser st Node
project = node
