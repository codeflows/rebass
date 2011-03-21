-- Attempt at parsing Reaper project files with Parsec

module ReaperProjectFileParser(ReaperProject(ReaperProject), parseProject) where

import Text.ParserCombinators.Parsec

data ReaperProject = ReaperProject Float String deriving (Show, Eq)

name :: CharParser st String
name = many1 (letter <|> char '_')

project :: CharParser st ReaperProject
project = do
  char '<'
  n <- name
  return $ ReaperProject 0.1 "3.73/OSX"

parseProject :: String -> Either ParseError ReaperProject
parseProject input = parse project "no source file" input
