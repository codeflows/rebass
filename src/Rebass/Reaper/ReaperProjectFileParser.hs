{-# LANGUAGE DeriveDataTypeable #-}
module Rebass.Reaper.ReaperProjectFileParser (project, parseProjectFile, ReaperParseException) where

import Rebass.Reaper.ReaperProject(Project, Node(Container, Command), Parameter(..))
import Text.ParserCombinators.Parsec
import Control.Exception hiding (try)
import Data.Typeable

parseProjectFile :: String -> IO Project
parseProjectFile file = do
  result <- parseFromFile project file
  case result of
    Left error -> throw $ ReaperParseException (show error)
    Right project -> return project

data NameAndParameters = NameAndParameters { name' :: String, parameters' :: [Parameter] }

data ReaperParseException = ReaperParseException String
  deriving (Show, Typeable)

instance Exception ReaperParseException 

project :: CharParser st Project
project = do
  n <- node
  many anyNewline
  eof
  return n

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
children = endBy (node <|> command) spaces

command :: CharParser st Node
command = fmap (Command `withNameAndParameters`) nameAndParameters

name :: CharParser st String
name = many1 (letter <|> digit <|> char '_')

parameters :: CharParser st [Parameter]
parameters = do
  p <- option [] parameterList
  newlines
  return p
  where
    parameterList = do
      char ' '
      sepBy1 parameter (char ' ')
    newlines =
      many1 anyNewline

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
string' = fmap String betweenAnyQuotes
  where
    betweenAnyQuotes = betweenQuotes '\'' <|> betweenQuotes '\"' <|> betweenQuotes '`'
    betweenQuotes quoteChar = between' quoteChar quoteChar

guid :: CharParser st Parameter
guid = do
  s <- between' '{' '}'
  return $ String ("{" ++ s ++ "}")

identifier :: CharParser st Parameter
identifier = fmap String name

anyNewline :: CharParser st Char
anyNewline = oneOf "\n\r"

between' :: Char -> Char -> CharParser st String
between' start end = between (char start) (char end) (many $ notChar end)
  where
    -- TODO does Parsec REALLY not have this function itself?
    notChar c = satisfy (/=c) <?> show [c]

withNameAndParameters :: (String -> [Parameter] -> t) -> NameAndParameters -> t
withNameAndParameters f np = f (name' np) (parameters' np)

