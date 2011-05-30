module Rebass.Cache where

import Rebass.Status
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import Rebass.Path
import Control.Monad(liftM)

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
    
statusFileFor name = rebassDir >>= returnSubPath name
	
getRemote = remoteConfigFile >>= readFile	

createRebassDir = rebassDir >>= createDirectoryIfMissing True

rebassDir = return ".rebass"

statusFile = rebassDir >>= returnSubPath "status"

remoteConfigFile = rebassDir >>= returnSubPath "remote"

returnSubPath file dir = return $ dir `subPath` file
