module Rebass.Reaper.Flatten(flatten, flattenSamples) where

import Rebass.Path
import System.Directory(doesFileExist)   
import Rebass.Reaper.Mp3(compressInto)  
import Rebass.ListUtil(replace)
import Rebass.Reaper.Samples
import Rebass.Reaper.ReaperProject(Project, Node(Command, Container), Parameter(..))
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile)

flatten :: Project -> Project
flatten (Container "SOURCE" 
            [String "WAVE"] 
            [Command "FILE" [String fileName]]) 
       = Container "SOURCE" 
            [String "MP3"] 
            [Command "FILE" [String flattenedAndConverted]]
  where flattenedAndConverted = (replace ".wav" ".mp3" $ lastPathElement fileName)
flatten l@(Command _ _) = l
flatten (Container name parameters children) = Container name parameters (map flatten children)
          
flattenSamples :: Path -> Path -> IO ()
flattenSamples projectPath dest = do
    project <- parseProjectFile projectPath
    mapM_ copySample (samples project)
        where copySample sample = ifNotExists (destFile sample) $ compressInto (sampleFilePath projectPath sample) $ destFile sample
              destFile sample = replace ".wav" ".mp3" (dest `subPath` ( lastPathElement $ fileName sample))
              ifNotExists file action = doesFileExist file >>= (\b -> if b then (return ()) else action)
   
