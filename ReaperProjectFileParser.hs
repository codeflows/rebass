-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (project) where

import ReaperProject(Project, Node(Container, Command), Parameter(..))
import Text.ParserCombinators.Parsec

data NameAndParameters = NameAndParameters { name' :: String, parameters' :: [Parameter] }

withNameAndParameters :: (String -> [Parameter] -> t) -> NameAndParameters -> t
withNameAndParameters f np = f (name' np) (parameters' np)

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

betweenQuotes :: Char -> CharParser st String
betweenQuotes quoteChar = between quote quote (many notQuote)
  where
    quote = char quoteChar
    notQuote = noneOf $ quoteChar : []

string' :: CharParser st Parameter
string' = do
  s <- betweenQuotes '\'' <|> betweenQuotes '\"'
  return $ String s

maybeMinusSign :: CharParser st String
maybeMinusSign = option "" (string "-")

decimal :: CharParser st Parameter
decimal = try decimal'
  where
    decimal' = do
      sign <- maybeMinusSign
      a <- many1 digit
      char '.'
      b <- many1 digit
      return $ Decimal (sign ++ a ++ '.' : b)

integer :: CharParser st Parameter
integer = do
  sign <- maybeMinusSign
  i <- many1 digit
  return $ Integer (read (sign ++ i) :: Integer)

parameter :: CharParser st Parameter
parameter = decimal <|> integer <|> string'

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

project :: CharParser st Project
project = node
