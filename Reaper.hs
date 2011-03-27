module Reaper where

import Path
import System.Directory

data Node = Leaf { name :: String, kind :: String }
            | Container { children :: [Node] }
            deriving (Show)
            
type Project = Node            
            
samples :: Project -> [Node]
samples s@(Leaf name "sample") = [s]
samples (Container children) = concat $ map samples children
samples _ = []

flatten :: Node -> Node
flatten (Leaf name "sample") = Leaf (lastPathElement name) "sample"
flatten l@(Leaf _ _) = l
flatten (Container children) = Container (map flatten children)

main = do 
    flattenSamples project dest
    where project = Container [Leaf "examples/patrolcar.wav" "sample"]
          sampleList = samples project
          dest = "/Users/juha/Dropbox/rebass/trololo"
          
flattenSamples :: Project -> Path -> IO ()
flattenSamples project dest = mapM_ copySample (samples project)
      where copySample sample = copyFile (name sample) (dest `subPath` (lastPathElement $ name sample))