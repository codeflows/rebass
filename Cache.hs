module Cache where

import Status
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Path

saveStatus :: Status -> IO ()
saveStatus status = do 
	 createRebassDir
	 writeFile statusFile $ show status

loadStatus :: IO Status     
loadStatus = do
     contents <- readFile statusFile
     let status = read contents :: Status
     return status

saveRemote :: String -> IO String
saveRemote name = do
    remote <- remoteLocationFor name
    createRebassDir
    createDirectoryIfMissing True remote
    putStrLn $ "Created directory " ++ remote
    writeFile remoteConfigFile remote
    return remote
	
remoteLocationFor name = do
    home <- getHomeDirectory	
    return $ home ++ "/Dropbox/Rebass/" ++ name
    
statusFileFor name = rebassDir `subPath` name    
	
getRemote = readFile remoteConfigFile	

createRebassDir = createDirectoryIfMissing True rebassDir

rebassDir = ".rebass"
statusFile = rebassDir ++ "/status"
remoteConfigFile = (rebassDir ++ "/remote")