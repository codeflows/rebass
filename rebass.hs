import Status
import ReadFiles
import Diff
import Cache
import UpdateFiles
import Compress
import ReaperStuff
import ReaperProjectFileParser(parseProjectFile)
import ReaperProjectFileSerializer(serialize)
import System.Environment(getArgs)
import System.Directory
import Path(subPath, lastPathElement)

main = getArgs >>= rebass
	
rebass :: [String] -> IO ()	

rebass ["init", projectFile, remoteAlias] = do
    project <- defineProject projectFile remoteAlias
    copyToRemote project
    status <- readCurrentStatus project
    writeStatus project status 
    putStrLn $ "Rebass initialized." 

rebass ["update", projectFile, remoteAlias] = do
    project <- defineProject projectFile remoteAlias
    cachedStatus <- readCachedStatus project
    currentStatus <- readCurrentStatus project
    let remoteChanged = (snd cachedStatus /= snd currentStatus)
    let localChanged = (fst cachedStatus /= fst currentStatus)
    if remoteChanged
        then putStrLn $ "Remote project changed. Update not implemented yet."
        else if localChanged
            then do 
              putStrLn $ "Local changes detected. Updating."
              copyToRemote project
              writeStatus project currentStatus
              putStrLn $ "Up to date."
            else putStrLn $ "Already up to date."    

rebass _ = do
  putStrLn "USAGE:"
  putStrLn "rebass init <projectfile> <remotealias>"
  putStrLn "rebass update <projectfile> <remotealias>"
  putStrLn "Example: rebass init examples/PatrolCar.RPP lollable"

data Project = Project { projectName :: String, 
                         localProjectFile :: String,
                         remoteAlias :: String, 
                         remoteLocation :: String, 
                         remoteProjectFile :: String} 

defineProject :: String -> String -> IO Project
defineProject projectFile remoteAlias = do
    let projectName = lastPathElement projectFile
    remoteLocation <- remoteLocationFor remoteAlias
    let remoteProjectFile = remoteLocation `subPath` projectName
    return $ Project projectName projectFile remoteAlias remoteLocation remoteProjectFile

readCurrentStatus project = do
    localStatus <- projectStatus $ localProjectFile project
    remoteStatus <- projectStatus $ remoteProjectFile project
    return (localStatus, remoteStatus)

readCachedStatus :: Project -> IO (ReaperProjectStatus, ReaperProjectStatus)
readCachedStatus project = do
    statusFile <- projectStatusFile project 
    readFile statusFile >>= (return . read)

writeStatus :: Project -> (ReaperProjectStatus, ReaperProjectStatus) -> IO ()
writeStatus project (localStatus, remoteStatus) = do
    statusFile <- projectStatusFile project 
    createRebassDir
    writeFile statusFile $ show (localStatus, remoteStatus)
    putStrLn $ "Using status file " ++ statusFile
    writeFile statusFile $ show (localStatus, remoteStatus)

projectStatusFile project = statusFileFor $ (projectName project) ++ ".status"

copyToRemote project = do
    createDirectoryIfMissing True $ remoteLocation project
    flattenSamples (localProjectFile project) $ remoteLocation	project
    parseProjectFile (localProjectFile project) >>= (writeFile $ remoteProjectFile project) . serialize . flatten
    putStrLn $ "Wrote project to " ++ (remoteLocation project)

