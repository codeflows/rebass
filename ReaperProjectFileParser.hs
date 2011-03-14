-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

-- TODO support any non-whitespace parameters
parameter = many1 letter

parameters = sepBy1 parameter (char ' ')

parameterList = do
                  char ' '
                  p <- parameters
                  newline
                  return p

noParameters = do
                newline
                return [[]]

maybeParameters = parameterList <|> noParameters

command = many1 (letter <|> char '_')

node = do
          char '<'
          c <- command
          p <- maybeParameters
          return (c, p)

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs

