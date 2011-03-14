import Status
import ReadFiles
import Diff
import Cache
import System.Environment(getArgs)


main = do
	args <- getArgs
	rebass args
	
rebass :: [String] -> IO ()	

rebass ("init" : name : args) = do
		saveRemote name
		newStatus <- readStatus $ "."
		saveStatus newStatus { contents = [] }
		putStrLn $ "Rebass initialized"
			
rebass ("update" : args) = do
		remote <- getRemote
		newStatus <- readStatus $ "."
		oldStatus <- loadStatus
		let diff = compareFile oldStatus newStatus
		putStrLn $ "Using remote repository '" ++ remote ++ "'"
		putStrLn $ "Changed since last rebass: " ++ show diff
		saveStatus newStatus        

rebass ("status" : args) = do
		newStatus <- readStatus $ "."
		oldStatus <- loadStatus
		let diff = compareFile oldStatus newStatus
		putStrLn $ "Changed since last rebass: " ++ show diff

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <name>"
	putStrLn "rebass update"
	putStrLn "rebass status"

