module ReaperProjectFileSerializer (serialize) where

import ReaperProject

serialize :: Node -> String
serialize = removeTrailingNewLine . serialize' 0
  where
    -- TODO is there a builtin function for this?
    removeTrailingNewLine s
      | last s == '\n' = init s
      | otherwise      = s

serialize' i (Command n p) =
  indent i ++ command n p
serialize' i (Container n p c) =
  indent i ++ "<" ++ command n p ++
    children (i + 2) c ++
  indent i ++ ">\n"

indent :: Int -> String
indent i = replicate i ' '

children :: Int -> [Node] -> String
children = concatMap . serialize'

command :: String -> [Parameter] -> String
command n p = unwords (n : map parameter p) ++ "\n"

parameter :: Parameter -> String
parameter (String s) = quote s
parameter (Integer i) = show i
parameter (Decimal d) = d

quote :: String -> String
quote s | contains singleQuote && contains doubleQuote = surround backTick
        | contains singleQuote                         = surround doubleQuote
        | contains doubleQuote                         = surround singleQuote
        | otherwise                                    = surround doubleQuote
  where
    singleQuote = '\''
    doubleQuote = '"'
    backTick = '`'
    contains char = elem char s
    surround char = [char] ++ s ++ [char]
