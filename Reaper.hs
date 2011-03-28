module Reaper where

import Path
import System.Directory   
import Compress(compressInto)  
import ListUtil(replace)

data Node = Leaf { name :: String, kind :: String }
            | Container { children :: [Node] }
            deriving (Show)
            
type Project = Node            
            
samples :: Project -> [Node]
samples s@(Leaf name "sample") = [s]
samples (Container children) = concat $ map samples children
samples _ = []

flatten :: Project -> Project
flatten (Leaf name "sample") = Leaf (lastPathElement name) "sample"
flatten l@(Leaf _ _) = l
flatten (Container children) = Container (map flatten children)

main = do 
    flattenSamples project dest
    -- TODO: flatten project too
  where project = Container [Leaf "examples/patrolcar.wav" "sample"]
        sampleList = samples project
        dest = "/Users/juha/Dropbox/rebass/trololo"
          
flattenSamples :: Project -> Path -> IO ()
flattenSamples project dest = do
        mapM_ copySample (samples project)
        
    where copySample sample = compressInto (name sample) $ replace ".wav" ".mp3" (dest `subPath` (lastPathElement $ name sample))