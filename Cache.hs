module Cache(saveStatus, loadStatus, saveRemote, getRemote) where

import Status
import System.Directory(createDirectoryIfMissing)

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
	writeFile remoteConfigFile remote
	
getRemote = readFile remoteConfigFile	

createRebassDir = createDirectoryIfMissing True rebassDir

rebassDir = ".rebass"
statusFile = rebassDir ++ "/status"
remoteConfigFile = (rebassDir ++ "/remote")