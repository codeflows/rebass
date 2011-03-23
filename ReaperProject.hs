module ReaperProject (
  Node(Container, Command),
  Parameter(..)
) where

data Parameter =
    String String
  | Integer Integer
  -- TODO precision?
  | Decimal Float
    deriving (Show, Eq)

data Node =
    Container { name :: String, parameters :: [Parameter], children :: [Node] }
  | Command { name :: String, parameters :: [Parameter] }
    deriving (Show, Eq)
