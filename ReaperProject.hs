module ReaperProject (
  Node(Container, Command),
  serialize
) where

data Node =
     Container { name :: String, parameters :: [String], children :: [Node] }
   | Command { name :: String, parameters :: [String] }
     deriving (Show, Eq)

serialize :: Node -> String
serialize (Command name parameters) = command name parameters
serialize (Container name parameters children) = "<" ++ command name parameters ++ ">"

command :: String -> [String] -> String
command n p = unwords(n:p) ++ "\n"

