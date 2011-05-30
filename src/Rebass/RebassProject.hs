module Rebass.RebassProject where

import Rebass.Reaper.ReaperProjectStatus
import Rebass.Path(subPath, lastPathElement)
import System.Directory(getHomeDirectory)

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

remoteLocationFor name = do
    home <- getHomeDirectory
    return $ home ++ "/Dropbox/Rebass/" ++ name

