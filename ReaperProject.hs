module ReaperProject(Node(Container, Command)) where

data Node =
     Container { name :: String, parameters :: [String], children :: [Node] }
   | Command { name :: String, parameters :: [String] }
     deriving (Show, Eq)
