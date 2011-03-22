import ReaperProjectFileParser
import Text.ParserCombinators.Parsec

main = do
  result <- parseFromFile project "Fake.RPP"
  case result of
    Left err  -> print err
    Right xs  -> print xs

