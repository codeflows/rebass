module ReaperProject where

data Command = Command { name :: String, parameters :: [String] }
  deriving (Show)

data Node = Container { command :: Command, children :: [Node] } | Leaf { command :: Command }
  deriving (Show)
