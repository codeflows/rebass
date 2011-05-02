module ReaperStuff where

import Path
import System.Directory   
import Compress(compressInto)  
import ListUtil(replace)
import ReaperProject(Project, Node(Command, Container), Parameter(..))
import ReaperProjectFileParser(parseProjectFile)
import System.Environment(getArgs)
import Status(File(RegularFile))
import Control.Monad
import ReadFiles(readFileStatus)

data Sample = Sample { fileName :: String }
instance Pathy Sample where
    pathOf sample = fileName sample

data ProjectStatus = ProjectStatus { projectFile :: File, projectSamples :: [File] }
    deriving (Show)

projectStatus :: Path -> IO ProjectStatus
projectStatus projectPath = liftM2 ProjectStatus (readFileStatus projectPath) readProjectSampleStatus
    where readProjectSampleStatus = parseProjectFile projectPath >>= projectSampleStatus
          projectSampleStatus = mapM (readFileStatus . (sampleFilePath projectPath)) . samples
            
samples :: Project -> [Sample]
samples (Command "FILE" [String fileName]) = [Sample fileName]
samples (Container name parameters children) = concat $ map samples children
samples _ = []

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
              
ifNotExists :: Path -> IO () -> IO ()
ifNotExists file action = doesFileExist file >>= ifElse (return ()) action
    
ifElse :: a -> a -> Bool -> a
ifElse a b True = a
ifElse a b False = b        
    
sampleFilePath projectPath sample | isAbsolutePath sample = fileName sample
                                  | otherwise             = (parent projectPath) `subPath` sample
    
