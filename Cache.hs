module Cache(saveStatus, loadStatus, saveRemote, getRemote, remoteLocationFor) where

import Status
import System.Directory(createDirectoryIfMissing, getHomeDirectory)

saveStatus :: File -> IO ()
saveStatus root = do 
	 createRebassDir
	 writeFile statusFile $ show root

loadStatus :: IO File     
loadStatus = do
     contents <- readFile statusFile
     let state = read contents :: File
     return state

saveRemote :: String -> IO ()
saveRemote remote = do
	createRebassDir
	createDirectoryIfMissing True remote
	putStrLn $ "Created directory " ++ remote
	writeFile remoteConfigFile remote
	
remoteLocationFor name = do
    home <- getHomeDirectory	
    return $ home ++ "/Dropbox/Rebass/" ++ name
	
getRemote = readFile remoteConfigFile	

createRebassDir = createDirectoryIfMissing True rebassDir

rebassDir = ".rebass"
statusFile = rebassDir ++ "/status"
remoteConfigFile = (rebassDir ++ "/remote")