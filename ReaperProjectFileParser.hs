-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

-- TODO support different types of parameters, e.g. decimal numbers, strings in quotes etc
parameter = many1 (noneOf " \n")

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
          char '>'
          return (c, p)

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs

