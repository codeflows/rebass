-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (project) where

import ReaperProject(Node(Container, Command), Parameter(..))
import Text.ParserCombinators.Parsec hiding (string)

data NameAndParameters = NameAndParameters { name' :: String, parameters' :: [Parameter] }

withNameAndParameters :: (String -> [Parameter] -> t) -> NameAndParameters -> t
withNameAndParameters f np = f (name' np) (parameters' np)

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

string :: CharParser st Parameter
string = do
  s <- between quote quote (many $ noneOf "\"")
  return $ String s
  where
    quote = char '"'

integer :: CharParser st Parameter
integer = do
  i <- many1 digit
  return $ Integer (read i :: Integer)

parameter :: CharParser st Parameter
parameter = string <|> integer

parameters :: CharParser st [Parameter]
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
  return $ Command `withNameAndParameters` np

children :: CharParser st [Node]
children = do
  endBy (node <|> command) spaces

node :: CharParser st Node
node = do
  char '<'
  np <- nameAndParameters
  spaces
  children <- children
  char '>'
  return $ (Container `withNameAndParameters` np) children

project :: CharParser st Node
project = node
