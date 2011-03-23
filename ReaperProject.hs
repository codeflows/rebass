module ReaperProject (
  Node(Container, Command),
  Parameter(..),
  serialize
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

-- TODO cleanup plz
serialize :: Node -> String
serialize = serialize' 0

serialize' i (Command n p) = indent i ++ command n p
serialize' i (Container n p c) =
  indent i ++ "<" ++ command n p ++
    children (i+2) c ++
  indent i ++ ">\n"
  where
    children i = concat . (map (serialize' i))

command n p = unwords (n:(map parameter p)) ++ "\n"
parameter :: Parameter -> String
parameter (String s) = s
indent i = replicate i ' '

