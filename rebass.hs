import Status
import ReadFiles
import Diff
import Cache
import UpdateFiles
import Compress
import ReaperStuff
import ReaperProjectFileParser(parseProjectFile)
import System.Environment(getArgs)
import System.Directory

main = getArgs >>= rebass
	
rebass :: [String] -> IO ()	

rebass ["init", projectFile, remoteAlias] = do
    remoteLocation <- remoteLocationFor remoteAlias
    status <- projectStatus projectFile
    createDirectoryIfMissing True remoteLocation
    flattenSamples projectFile remoteLocation	
    putStrLn $ "Rebass initialized. Using remote repository " ++ remoteLocation

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <projectfile> <remotelocation>"
	putStrLn "rebass update"
	putStrLn "rebass status"