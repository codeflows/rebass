module ReaperProject (
  Project,
  Node(Container, Command),
  Parameter(..)
) where

data Node =
    Container { name :: String, parameters :: [Parameter], children :: [Node] }
  | Command { name :: String, parameters :: [Parameter] }
    deriving (Show, Eq)

type Project = Node

data Parameter =
    String String
  | Integer Integer
  | Decimal String
    deriving (Show, Eq)
