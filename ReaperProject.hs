module ReaperProject(Node(..)) where

data Node =
     Container { command :: String, parameters :: [String], children :: [Node] }
-- TODO Leaf -> Command?
   | Leaf { command :: String, parameters :: [String] }
     deriving (Show, Eq)
