module ReaperProject(Node(..)) where

data Node =
     Container { name :: String, parameters :: [String], children :: [Node] }
   | Leaf { name :: String, parameters :: [String] :: Command }
     deriving (Show, Eq)
