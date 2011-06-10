module Rebass.Reaper.ReaperProject (
  Project,
  Node(Container, Command, Chunk),
  Parameter(..)
) where

data Node =
    Container { name :: String, parameters :: [Parameter], children :: [Node] }
  | Command { name :: String, parameters :: [Parameter] }
  | Chunk { content :: String }
    deriving (Show, Eq)

type Project = Node

data Parameter =
    String String
  | Integer Integer
  | Decimal String
    deriving (Show, Eq)
