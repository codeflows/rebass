module Rebass.RebassProject where

import Rebass.FileStatus
import Rebass.Cache
import Rebass.Reaper.Samples
import Rebass.Reaper.Flatten
import Rebass.Reaper.ReaperProjectStatus
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile)
import Rebass.Reaper.ReaperProjectFileSerializer(serialize)
import System.Environment(getArgs)
import System.Directory
import Rebass.Path(subPath, lastPathElement)

data RebassProject = RebassProject { projectName :: String, 
	                 localProjectFile :: String,
	                 remoteAlias :: String, 
	                 remoteLocation :: String, 
	                 remoteProjectFile :: String} 

data RebassProjectStatus = RebassProjectStatus { localStatus :: ReaperProjectStatus,
		         alias :: String,
		         remoteStatus :: ReaperProjectStatus }
		       deriving(Read, Show, Eq)

projectNameFromFileName = lastPathElement

defineProject :: String -> String -> IO RebassProject
defineProject projectFile remoteAlias = do
    let projectName = projectNameFromFileName projectFile
    remoteLocation <- remoteLocationFor remoteAlias
    let remoteProjectFile = remoteLocation `subPath` projectName
    return $ RebassProject projectName projectFile remoteAlias remoteLocation remoteProjectFile

readCurrentStatus project = do
    localStatus <- projectStatus $ localProjectFile project
    remoteStatus <- projectStatus $ remoteProjectFile project
    return $ RebassProjectStatus localStatus (remoteAlias project) remoteStatus

readCachedStatus :: String -> IO RebassProjectStatus
readCachedStatus projectName = do
    statusFile <- projectStatusFile projectName
    readFile statusFile >>= (return . read)

writeStatus :: RebassProject -> RebassProjectStatus -> IO ()
writeStatus project status = do
    statusFile <- projectStatusFile $ projectName project 
    createRebassDir
    putStrLn $ "Using status file " ++ statusFile
    writeFile statusFile $ show status 

projectStatusFile projectName = statusFileFor $ projectName ++ ".status"
