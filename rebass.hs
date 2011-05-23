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

rebass ["update", remoteAlias] = do
    putStrLn $ "Update not yet implemented!"

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <projectfile> <remotealias>"
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

writeStatus project (localStatus, remoteStatus) = do
    statusFile <- statusFileFor $ (remoteAlias project) ++ "." ++ (projectName project) ++ ".status"
    createRebassDir
    writeFile statusFile $ show (localStatus, remoteStatus)
    putStrLn $ "Using status file " ++ statusFile
    writeFile statusFile $ show (localStatus, remoteStatus)

copyToRemote project = do
    createDirectoryIfMissing True $ remoteLocation project
    flattenSamples (localProjectFile project) $ remoteLocation	project
    parseProjectFile (localProjectFile project) >>= (writeFile $ remoteProjectFile project) . serialize . flatten

