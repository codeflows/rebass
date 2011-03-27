module Reaper where

import Path

data Node = Leaf { name :: String, kind :: String }
            | Container { children :: [Node] }
            deriving (Show)
            
samples :: Node -> [Node]
samples s@(Leaf name "sample") = [s]
samples (Container children) = concat $ map samples children
samples _ = []

flatten :: Node -> Node
flatten (Leaf name "sample") = Leaf (lastPathElement name) "sample"
flatten l@(Leaf _ _) = l
flatten (Container children) = Container (map flatten children)