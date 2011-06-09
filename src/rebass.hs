{-# LANGUAGE ScopedTypeVariables #-}

import Rebass.RebassProject
import Rebass.FileStatus
import Rebass.Cache
import Rebass.Reaper.Samples
import Rebass.Reaper.Flatten
import Rebass.Reaper.ReaperProjectStatus
import Rebass.Reaper.ReaperProjectFileParser(parseProjectFile, ReaperParseException)
import Rebass.Reaper.ReaperProjectFileSerializer(serialize)
import System.Environment(getArgs)
import System.Directory
import Rebass.Path(subPath, lastPathElement)
import Control.Exception
import Prelude hiding (catch)

main = getArgs >>= rebassWithErrorHandling

rebassWithErrorHandling args = (rebass args) 
  `catches` [Handler(\ (e :: IOException) -> logError "IO error" e),
             Handler(\ (e :: ReaperParseException) -> logError "Parsing error" e)]
  where logError kind ex = putStrLn $ kind ++ " : " ++ show ex
	
rebass :: [String] -> IO ()	

rebass ["init", projectFile, remoteAlias] = do
    project <- defineProject projectFile remoteAlias
    copyToRemote project
    status <- readCurrentStatus project
    writeStatus project status 
    putStrLn "Rebass initialized." 

rebass ["update", projectFile] = do
    cachedStatus <- readCachedStatus $ projectNameFromFileName projectFile
    project <- defineProject projectFile $ alias cachedStatus
    currentStatus <- readCurrentStatus project
    let remoteChanged = (remoteStatus cachedStatus /= remoteStatus currentStatus)
    let localChanged = (localStatus cachedStatus /= localStatus currentStatus)
    if remoteChanged
        then putStrLn "Remote project changed. Update not implemented yet."
        else if localChanged
            then do 
              putStrLn "Local changes detected. Updating."
              copyToRemote project
              writeStatus project currentStatus
              putStrLn "Up to date."
            else putStrLn "Already up to date."    

rebass _ = do
  putStrLn "USAGE:"
  putStrLn "rebass init <projectfile> <remotealias>"
  putStrLn "rebass update <projectfile>" 
  putStrLn "Example: rebass init examples/PatrolCar.RPP lollable"

copyToRemote project = do
    createDirectoryIfMissing True $ remoteLocation project
    flattenSamples (localProjectFile project) $ remoteLocation	project
    parseProjectFile (localProjectFile project) >>= writeFile (remoteProjectFile project) . serialize . flatten
    putStrLn $ "Wrote project to " ++ remoteLocation project

