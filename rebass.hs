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
	newStatus <- readFiles "." remote
	oldStatus <- loadStatus
	let diff = compareStatus oldStatus newStatus
	let conflicts = getConflicts diff
	resolveConflictsAndUpdate conflicts remote diff
  where 
    resolveConflictsAndUpdate [] remote diff = do
    	updateFiles "." remote $ fst diff
    	readFiles "." remote >>= saveStatus
    resolveConflictsAndUpdate conflicts _ _ = do
    	putStrLn $ "Conflicts : " ++ show conflicts    

rebass ("status" : _) = do
    newStatus <- getRemote >>= (readFiles ".")
    oldStatus <- loadStatus
    let diff = compareStatus oldStatus newStatus    
    showDiff "remote repository" $ snd diff
    showDiff "local files" $ fst diff
  where
    showDiff name diff = do
    	putStrLn $ "Changes in " ++ name ++ ": " ++ show diff    

rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <name>"
	putStrLn "rebass update"
	putStrLn "rebass status"

