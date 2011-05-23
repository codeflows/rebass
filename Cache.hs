module Cache where

import Status
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Path
import Control.Monad(liftM)

saveStatus :: Status -> IO ()
saveStatus status = do
   createRebassDir
   sft <- statusFile
   writeFile sft $ show status

loadStatus :: IO Status
loadStatus = do
     contents <- statusFile >>= readFile 
     let status = read contents :: Status
     return status

saveRemote :: String -> IO String
saveRemote name = do
    remote <- remoteLocationFor name
    createRebassDir
    createDirectoryIfMissing True remote
    putStrLn $ "Created directory " ++ remote
    remoteConfig <- remoteConfigFile
    writeFile remoteConfig remote
    return remote

remoteLocationFor name = do
    home <- getHomeDirectory
    return $ home ++ "/Dropbox/Rebass/" ++ name
    
statusFileFor name = rebassDir >>= (containing name)    
	
getRemote = remoteConfigFile >>= readFile	

createRebassDir = do
    dir <- rebassDir
    createDirectoryIfMissing True dir

rebassDir = getHomeDirectory >>= (containing ".rebass")

statusFile = rebassDir >>= (containing "status")

remoteConfigFile = rebassDir >>= (containing "remote")

containing file dir = return $ dir `subPath` file
