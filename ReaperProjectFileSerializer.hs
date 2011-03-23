-- TODO clean up
module ReaperProjectFileSerializer (serialize) where

import ReaperProject

serialize :: Node -> String
serialize = serialize' 0

serialize' i (Command n p) = indent i ++ command n p
serialize' i (Container n p c) =
  indent i ++ "<" ++ command n p ++
    children (i+2) c ++
  indent i ++ ">\n"
  where
    children i = concat . (map (serialize' i))

command n p = unwords (n:(map parameter p)) ++ "\n"
parameter :: Parameter -> String
parameter (String s) = s
indent i = replicate i ' '

