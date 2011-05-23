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
    dumpProject projectFile remoteAlias
    status <- readStatus projectFile remoteAlias
    writeStatus projectFile remoteAlias status 
    putStrLn $ "Rebass initialized." 

rebass ["update", remoteAlias] = do
    putStrLn $ "Update not yet implemented!"

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <projectfile> <remotealias>"
	putStrLn "Example: rebass init examples/PatrolCar.RPP lollable"

readStatus projectFile remoteAlias = do
    remoteLocation <- remoteLocationFor remoteAlias
    let projectName = lastPathElement projectFile
    let remoteProjectFile = remoteLocation `subPath` projectName
    localStatus <- projectStatus projectFile
    remoteStatus <- projectStatus remoteProjectFile
    return (localStatus, remoteStatus)

writeStatus projectFile remoteAlias (localStatus, remoteStatus) = do
    let projectName = lastPathElement projectFile
    statusFile <- statusFileFor $ remoteAlias ++ "." ++ projectName ++ ".status"
    createRebassDir
    writeFile statusFile $ show (localStatus, remoteStatus)
    putStrLn $ "Using status file " ++ statusFile
    writeFile statusFile $ show (localStatus, remoteStatus)

dumpProject projectFile remoteAlias = do
    remoteLocation <- remoteLocationFor remoteAlias
    let projectName = lastPathElement projectFile
    createDirectoryIfMissing True remoteLocation
    flattenSamples projectFile remoteLocation	
    let remoteProjectFile = remoteLocation `subPath` projectName
    parseProjectFile projectFile >>= (writeFile remoteProjectFile) . serialize . flatten

