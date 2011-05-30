module Rebass.Reaper.ReaperProjectStatus(ReaperProjectStatus, projectStatus) where

import Rebass.Path
import System.Directory   
import Rebass.ListUtil(replace)
import Rebass.Reaper.Samples(samples, sampleFilePath)
import Rebass.Reaper.ReaperProject(Project, Node(Command, Container), Parameter(..))
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile)
import System.Environment(getArgs)
import Rebass.FileStatus(File(RegularFile), readFileStatus)
import Control.Monad

data ReaperProjectStatus = ReaperProjectStatus { projectFile :: File, projectSamples :: [File] }
    deriving (Show, Read, Eq)

projectStatus :: Path -> IO ReaperProjectStatus
projectStatus projectPath = liftM2 ReaperProjectStatus (readFileStatus projectPath) readProjectSampleStatus
    where readProjectSampleStatus = parseProjectFile projectPath >>= projectSampleStatus
          projectSampleStatus = mapM (readFileStatus . (sampleFilePath projectPath)) . samples


