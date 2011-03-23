module ReaperProject (
  Node(Container, Command),
  Parameter(..),
  serialize
) where

data Parameter =
    String String
  | Integer Integer
  | Decimal Float
    deriving (Show, Eq)

data Node =
    Container { name :: String, parameters :: [Parameter], children :: [Node] }
  | Command { name :: String, parameters :: [Parameter] }
    deriving (Show, Eq)

-- TODO cleanup plz
serialize :: Node -> String
serialize = print 0
  where
    print i (Command n p) = indent i ++ command n p
    print i (Container n p c) = indent i ++ "<" ++ command n p ++ children (i+2) c ++ indent i ++ ">\n"
    children i = concat . (map (print i))
    command n p = unwords (n:(map show p)) ++ "\n"
    indent i = replicate i ' '

