import Rebass.Reaper.ReaperProjectFileParser
import Rebass.Reaper.ReaperProjectFileSerializer
import System.Environment(getArgs)
import Text.ParserCombinators.Parsec(parseFromFile)

main = getArgs >>= parse

parse [file] = do
  result <- parseFromFile project file
  case result of
    Left err  -> print err
    Right project  -> putStrLn (serialize project)
