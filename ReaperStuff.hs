module Reaper where

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
          projectSampleStatus = mapM (readFileStatus . sampleFilePath) . samples
          sampleFilePath sample | isAbsolutePath sample = fileName sample
                                | otherwise             = (parent projectPath) `subPath` sample
            
samples :: Project -> [Sample]
samples (Command "FILE" [String fileName]) = [Sample fileName]
samples (Container name parameters children) = concat $ map samples children
samples _ = []

flatten :: Project -> Project
flatten (Command "FILE" [String fileName]) = Command "FILE" [String (lastPathElement fileName)]
flatten l@(Command _ _) = l
flatten (Container name parameters children) = Container name parameters (map flatten children)

main = do
    status <- projectStatus "examples/PatrolCar.RPP"
    putStrLn $ show status
--main = getArgs >>= dumpProject
        
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