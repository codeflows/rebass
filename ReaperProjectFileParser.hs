-- Attempt at parsing Reaper project files with Parsec

module Reaper where

import Text.ParserCombinators.Parsec

{-csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'-}

reaperNode = char '<'

main    = do{ result <- parseFromFile reaperNode "Fake.RPP"
              ; case result of
                  Left err  -> print err
                  Right xs  -> print xs
              }

