-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

{-csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'-}

parameter = many1 letter

parameters = do
              char ' '
              p <- sepBy1 parameter (char ' ')
              newline
              return p

noParameters = do
                newline
                return [[]]

command = many1 (letter <|> char '_')

node = do
          char '<'
          c <- command
          x <- parameters <|> noParameters
          return (c, x)

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs

