import ReaperProjectFileParser
import System.Environment(getArgs)
import Text.ParserCombinators.Parsec(parseFromFile)

main = getArgs >>= parse

parse [file] = do
  result <- parseFromFile project file
  case result of
    Left err  -> print err
    Right xs  -> print xs

