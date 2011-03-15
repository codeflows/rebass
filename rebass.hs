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
	saveStatus $ Status emptyDir emptyDir
	putStrLn $ "Rebass initialized. Using remote repository " ++ remote
			
rebass ("update" : _) = do
	remote <- getRemote
	putStrLn $ "Using remote repository '" ++ remote ++ "'"
	newStatus <- readStatus "." remote
	oldStatus <- loadStatus
	let localDiff = compareFile (localStatus oldStatus) (localStatus newStatus)
	let remoteDiff = compareFile (remoteStatus oldStatus) (remoteStatus newStatus)
	let conflicts = getConflicts remoteDiff localDiff
	resolveConflictsAndUpdate conflicts remote localDiff newStatus
  where 
    resolveConflictsAndUpdate [] remote localDiff newStatus = do
    	updateFiles "." remote localDiff
    	saveStatus newStatus        
    resolveConflictsAndUpdate conflicts _ _ _ = do
    	putStrLn $ "Conflicts : " ++ show conflicts    

rebass ("status" : _) = do
    newStatus <- getRemote >>= (readStatus ".")
    oldStatus <- loadStatus
    showDiff "remote repository" (remoteStatus oldStatus) (remoteStatus newStatus)
    showDiff "local files" (localStatus oldStatus) (localStatus newStatus)
  where
    showDiff name old new = do
    	let diff = compareFile old new
    	putStrLn $ "Changes in " ++ name ++ ": " ++ show diff    

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <name>"
	putStrLn "rebass update"
	putStrLn "rebass status"

