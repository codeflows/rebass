module Rebass.Reaper.ReaperProjectSync(flatten, flattenSamples) where

import Rebass.Diff(Diff, diff)
import Rebass.Path
import System.Directory   
import Rebass.Compress(compressInto)  
import Rebass.ListUtil(replace)
import Rebass.Reaper.Samples
import Rebass.Reaper.ReaperProject(Project, Node(Command, Container), Parameter(..))
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile)
import System.Environment(getArgs)
import Rebass.Status(File(RegularFile))
import Control.Monad
import Rebass.ReadFiles(readFileStatus)

flatten :: Project -> Project
flatten (Command "FILE" [String fileName]) = Command "FILE" [String (replace ".wav" ".mp3" $ lastPathElement fileName)]
flatten l@(Command _ _) = l
flatten (Container name parameters children) = Container name parameters (map flatten children)
          
flattenSamples :: Path -> Path -> IO ()
flattenSamples projectPath dest = do
    project <- parseProjectFile projectPath
    mapM_ copySample (samples project)
        where copySample sample = ifNotExists (destFile sample) $ compressInto (sampleFilePath projectPath sample) $ destFile sample
              destFile sample = replace ".wav" ".mp3" (dest `subPath` ( lastPathElement $ fileName sample))
              ifNotExists file action = doesFileExist file >>= (\b -> if b then (return ()) else action)
   
