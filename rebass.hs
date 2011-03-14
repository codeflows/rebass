import Status
import ReadFiles
import Diff
import Cache
import System.Environment(getArgs)


main = do
	args <- getArgs
	rebass args
	
rebass :: [String] -> IO ()	

rebass [] = do
	putStrLn $ "usage: rebass init|update"

rebass ("init" : args) = do
		newStatus <- readStatus $ "."
		saveStatus newStatus
		putStrLn $ "Rebass initialized"
			
rebass ("update" : args) = do
        newStatus <- readStatus $ "."
        oldStatus <- loadStatus
        let diff = compareFile oldStatus newStatus
        putStrLn $ "Changed since last rebass: " ++ show diff
        saveStatus newStatus        

