module ReaperProject(Node(..), Command(..)) where

data Command =
     Command { name :: String, parameters :: [String] }
     deriving (Show, Eq)

data Node =
     Container { command :: Command, children :: [Node] }
   | Leaf { command :: Command }
     deriving (Show, Eq)
