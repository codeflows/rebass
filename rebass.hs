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
    remoteLocation <- remoteLocationFor remoteAlias
    status <- projectStatus projectFile
    createDirectoryIfMissing True remoteLocation
    flattenSamples projectFile remoteLocation	
    let remoteProjectFile = remoteLocation `subPath` (lastPathElement projectFile)
    parseProjectFile projectFile >>= (writeFile remoteProjectFile) . serialize . flatten
    putStrLn $ "Rebass initialized. Using remote repository " ++ remoteLocation

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <projectfile> <remotealias>"
	putStrLn "Example: rebass init examples/PatrolCar.RPP lollable"