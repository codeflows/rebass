module ReaperProject (
  Node(Container, Command),
  serialize
) where

data Node =
     Container { name :: String, parameters :: [String], children :: [Node] }
   | Command { name :: String, parameters :: [String] }
     deriving (Show, Eq)

serialize :: Node -> String
serialize (Command n p) = command n p
serialize (Container n p c) = "<" ++ command n p ++ children c ++ ">\n"
  where
    children = concat . (map serialize)

command :: String -> [String] -> String
command n p = unwords (n:p) ++ "\n"

