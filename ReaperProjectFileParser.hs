-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

{-csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'-}

parameter = many1 letter

parameters = sepBy1 parameter (char ' ')

command = many1 (letter <|> char '_')

node = do
          char '<'
          c <- command
          x <- char ' ' <|> newline
          --p <- sepEndBy parameter (char ' ')
          return (c, x)

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs

