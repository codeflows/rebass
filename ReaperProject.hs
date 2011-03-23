module ReaperProject (
  Node(Container, Command),
  serialize
) where

data Node =
     Container { name :: String, parameters :: [String], children :: [Node] }
   | Command { name :: String, parameters :: [String] }
     deriving (Show, Eq)

serialize :: Node -> String
serialize = print 0
  where
    print i (Command n p) = indent i ++ command n p
    print i (Container n p c) = indent i ++ "<" ++ command n p ++ children (i+2) c ++ indent i ++ ">\n"
    children i = concat . (map (print i))
    command n p = unwords (n:p) ++ "\n"
    indent i = replicate i ' '

