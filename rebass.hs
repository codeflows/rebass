import Status
import ReadFiles
import Diff
import Cache
import UpdateFiles
import Compress
import System.Environment(getArgs)

main = getArgs >>= rebass
	
rebass :: [String] -> IO ()	

rebass ["init", name] = do
	remote <- saveRemote name
	saveStatus $ Status emptyDir emptyDir
	putStrLn $ "Rebass initialized. Using remote repository " ++ remote
			
rebass ["update"] = do
	diff <- calculateDiff
	resolveConflictsAndUpdate (getConflicts diff) diff
  where 
    resolveConflictsAndUpdate [] diff = do
        remote <- getRemote
        compress "." $ fst diff
        updateFiles "." remote $ fst diff
        updateFiles remote "." $ snd diff
    	readFiles "." remote >>= saveStatus
    resolveConflictsAndUpdate conflicts _ = do
    	putStrLn $ "Conflicts : " ++ show conflicts    

rebass ["status"] = do
    diff <- calculateDiff
    showDiff "remote repository" $ snd diff
    showDiff "local files" $ fst diff
    putStrLn $ "Conflicts : " ++ show (getConflicts diff)
  where
    showDiff name diff = do
    	putStrLn $ "Changes in " ++ name ++ ": " ++ show diff    
    	
rebass _ = do
	putStrLn "USAGE:"
	putStrLn "rebass init <name>"
	putStrLn "rebass update"
	putStrLn "rebass status"

calculateDiff = do    	
    newStatus <- getRemote >>= (readFiles ".")
    oldStatus <- loadStatus
    return $ compareStatus oldStatus newStatus