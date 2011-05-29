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

rebass ["update", projectFile] = do
    cachedStatus <- readCachedStatus $ projectNameFromFileName projectFile
    project <- defineProject projectFile $ alias cachedStatus
    currentStatus <- readCurrentStatus project
    let remoteChanged = (remoteStatus cachedStatus /= remoteStatus currentStatus)
    let localChanged = (localStatus cachedStatus /= localStatus currentStatus)
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
  putStrLn "rebass update <projectfile>" 
  putStrLn "Example: rebass init examples/PatrolCar.RPP lollable"

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

copyToRemote project = do
    createDirectoryIfMissing True $ remoteLocation project
    flattenSamples (localProjectFile project) $ remoteLocation	project
    parseProjectFile (localProjectFile project) >>= (writeFile $ remoteProjectFile project) . serialize . flatten
    putStrLn $ "Wrote project to " ++ (remoteLocation project)

