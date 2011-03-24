module ReaperProjectFileSerializer (serialize) where

import ReaperProject

serialize :: Node -> String
serialize = removeTrailingNewLine . (serialize' 0)
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
children i = concat . (map (serialize' i))

command :: String -> [Parameter] -> String
command n p = unwords (n:(map parameter p)) ++ "\n"

parameter :: Parameter -> String
parameter (String s) = "\"" ++ s ++ "\""
parameter (Integer i) = show i
parameter (Decimal d) = d

