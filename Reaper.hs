module Reaper where

import Path
import System.Directory   
import Compress(compressInto)  
import ListUtil(replace)
import ReaperProject
import System.Environment(getArgs)

data Sample = Sample { fileName :: String }
            
samples :: Project -> [Sample]
samples (Command "FILE" [String fileName]) = [Sample fileName]
samples (Container name parameters children) = concat $ map samples children
samples _ = []

flatten :: Project -> Project
flatten (Command "FILE" [String fileName]) = Command "FILE" [String (lastPathElement fileName)]
flatten l@(Command _ _) = l
flatten (Container name parameters children) = Container name parameters (map flatten children)

main = getArgs >>= dumpProject
        
dumpProject [dest] = do 
    createDirectoryIfMissing True dest
    flattenSamples project dest
    -- TODO: flatten project too
  where project = Container "project" [] [Command "FILE" [String "examples/patrolcar.wav"]]
        sampleList = samples project  
dumpProject _ = putStrLn "Plz provide dest dir"            
          
flattenSamples :: Project -> Path -> IO ()
flattenSamples project dest = do
        mapM_ copySample (samples project)        
    where copySample sample = compressInto (fileName sample) $ replace ".wav" ".mp3" (dest `subPath` (lastPathElement $ fileName sample))