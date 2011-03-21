-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

node = do
  char '<'

parseNode input = parse node "lolcat" input

main = do
          result <- parseFromFile node "Fake.RPP"
          case result of
            Left err  -> print err
            Right xs  -> print xs
