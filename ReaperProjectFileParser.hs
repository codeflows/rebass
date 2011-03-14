-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

-- TODO support any non-whitespace parameters
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

