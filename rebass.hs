import Status
import ReadFiles
import Diff
import Cache
import UpdateFiles
import System.Environment(getArgs)

main = getArgs >>= rebass
	
rebass :: [String] -> IO ()	

rebass ("init" : name : _) = do
	remote <- saveRemote name
	newStatus <- readStatus "."
	saveStatus newStatus { contents = [] }
	putStrLn $ "Rebass initialized. Using remote repository " ++ remote
			
rebass ("update" : _) = do
	remote <- getRemote
	putStrLn $ "Using remote repository '" ++ remote ++ "'"
	newLocalStatus <- readStatus "."
	newRemoteStatus <- readStatus remote
	oldStatus <- loadStatus
	let localDiff = compareFile oldStatus newLocalStatus
	let remoteDiff = compareFile oldStatus newRemoteStatus
	let conflicts = getConflicts remoteDiff localDiff
	resolveConflictsAndUpdate conflicts remote localDiff newLocalStatus
  where 
    resolveConflictsAndUpdate [] remote localDiff newLocalStatus = do
    	updateFiles "." remote localDiff
    	saveStatus newLocalStatus        
    resolveConflictsAndUpdate conflicts _ _ _ = do
    	putStrLn $ "Conflicts : " ++ show conflicts    

rebass ("status" : _) = do
	newStatus <- readStatus "."
	oldStatus <- loadStatus
	let diff = compareFile oldStatus newStatus
	putStrLn $ "Changed since last rebass: " ++ show diff

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <name>"
	putStrLn "rebass update"
	putStrLn "rebass status"

