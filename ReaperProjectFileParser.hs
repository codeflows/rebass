-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser (project) where

import ReaperProject(Project, Node(Container, Command), Parameter(..))
import Text.ParserCombinators.Parsec

data NameAndParameters = NameAndParameters { name' :: String, parameters' :: [Parameter] }

project :: CharParser st Project
project = node

node :: CharParser st Node
node = do
  char '<'
  np <- nameAndParameters
  spaces
  children <- children
  char '>'
  return $ (Container `withNameAndParameters` np) children

nameAndParameters :: CharParser st NameAndParameters
nameAndParameters = do
  n <- name
  p <- parameters
  return $ NameAndParameters n p

children :: CharParser st [Node]
children = do
  endBy (node <|> command) spaces

command :: CharParser st Node
command = do
  np <- nameAndParameters
  return $ Command `withNameAndParameters` np

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

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

parameter :: CharParser st Parameter
parameter = decimal <|> integer <|> string' <|> guid <|> identifier

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

maybeMinusSign :: CharParser st String
maybeMinusSign = option "" (string "-")

string' :: CharParser st Parameter
string' = do
  s <- betweenQuotes '\'' <|> betweenQuotes '\"' <|> betweenQuotes '`'
  return $ String s
  where
    betweenQuotes quoteChar = between' quoteChar quoteChar

guid :: CharParser st Parameter
guid = do
  s <- between' '{' '}'
  return $ String ("{" ++ s ++ "}")

identifier :: CharParser st Parameter
identifier = do
  id <- name
  return $ String id

between' :: Char -> Char -> CharParser st String
between' start end = between (char start) (char end) (many $ notChar end)
  where
    -- TODO does Parsec REALLY not have this function itself?
    notChar c = satisfy (/=c) <?> show [c]

withNameAndParameters :: (String -> [Parameter] -> t) -> NameAndParameters -> t
withNameAndParameters f np = f (name' np) (parameters' np)

