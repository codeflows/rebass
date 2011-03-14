-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

data Command = Command String [String] deriving (Show)
data Node = Node Command [Command] deriving (Show)

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

name = many1 (letter <|> char '_')

command = do
            n <- name
            p <- maybeParameters
            return $ Command n p

child = do
          -- TODO getting unelegant
          many (char ' ')
          command

node = do
          char '<'
          mainCommand <- command
          subCommands <- many child
          char '>'
          return $ Node mainCommand subCommands

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs
