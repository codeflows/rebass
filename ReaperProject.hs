module ReaperProject(Node(..), Command(..)) where

data Command =
     Command { name :: String, parameters :: [String] }
     deriving (Show)

data Node =
     Container { command :: Command, children :: [Command] }
   | Leaf { command :: Command }
     deriving (Show)
