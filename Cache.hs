module Cache where

import Status
import System.Directory(createDirectoryIfMissing)

saveStatus :: File -> IO ()
saveStatus root = do 
	 createDirectoryIfMissing True rebassDir
	 writeFile statusFile $ show root

loadStatus :: IO File     
loadStatus = do
     contents <- readFile statusFile
     let state = read contents :: File
     return state

rebassDir = ".rebass"
statusFile = rebassDir ++ "/status"